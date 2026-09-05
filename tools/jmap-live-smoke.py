#!/usr/bin/env python3
"""Bounded, read-only JMAP Session and Mail smoke test.

Credentials are read from a private file and written only to a mode-0600 curl
configuration in a mode-0700 temporary directory.  They are never placed in
argv, environment variables, stdout, or the repository.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import stat
import subprocess
import tempfile
from typing import Any
from urllib.parse import urljoin, urlsplit

CORE_CAPABILITY = "urn:ietf:params:jmap:core"
MAIL_CAPABILITY = "urn:ietf:params:jmap:mail"
SUBMISSION_CAPABILITY = "urn:ietf:params:jmap:submission"


def authority(url: str) -> tuple[str | None, int]:
    """Return normalized host/port authority for URL."""
    parts = urlsplit(url)
    return parts.hostname, parts.port or (443 if parts.scheme == "https" else 80)


def validate_https_url(url: str, *, context: str) -> None:
    """Reject non-HTTPS URLs and URL userinfo."""
    parts = urlsplit(url)
    if (
        parts.scheme != "https"
        or not parts.hostname
        or parts.username
        or parts.password
    ):
        raise RuntimeError(f"unsafe {context} URL: {url}")


def curl_config_quote(value: str) -> str:
    """Return VALUE safely quoted for one curl config argument."""
    if "\0" in value:
        raise RuntimeError("curl config value contains NUL")
    escaped = (
        value.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\t", "\\t")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\v", "\\v")
    )
    return f'"{escaped}"'


def read_password(path: Path) -> str:
    """Read a private password file without logging its contents."""
    info = path.stat()
    if info.st_uid != os.getuid():
        raise RuntimeError(f"password file is not owned by the current uid: {path}")
    if stat.S_IMODE(info.st_mode) & 0o077:
        raise RuntimeError(f"password file must not be group/world accessible: {path}")
    password = path.read_text(encoding="utf-8").rstrip("\r\n")
    if not password:
        raise RuntimeError("password file is empty")
    return password


class CurlJsonClient:
    """Small bounded curl adapter with manual redirect handling."""

    def __init__(self, user: str, password: str) -> None:
        self.user = user
        self.password = password

    def request(
        self,
        url: str,
        *,
        body: dict[str, Any] | None,
        byte_cap: int,
    ) -> tuple[dict[str, Any], str, str | None]:
        validate_https_url(url, context="request")
        with tempfile.TemporaryDirectory(prefix="chidu-jmap-smoke-") as directory:
            root = Path(directory)
            os.chmod(root, 0o700)
            config_path = root / "curl.conf"
            output_path = root / "body"
            headers_path = root / "headers"
            request_path = root / "request.json"
            lines = [
                f"url = {curl_config_quote(url)}",
                f"user = {curl_config_quote(f'{self.user}:{self.password}')}",
                "silent",
                "show-error",
                "fail-with-body",
                'proto = "=https"',
                "connect-timeout = 10",
                "max-time = 30",
                f"max-filesize = {byte_cap}",
                f"dump-header = {curl_config_quote(str(headers_path))}",
                f"output = {curl_config_quote(str(output_path))}",
                'header = "Accept: application/json"',
            ]
            if body is not None:
                request_path.write_text(
                    json.dumps(body, separators=(",", ":")), encoding="utf-8"
                )
                os.chmod(request_path, 0o600)
                lines += [
                    'request = "POST"',
                    'header = "Content-Type: application/json"',
                    f"data-binary = {curl_config_quote(f'@{request_path}')}",
                ]
            config_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
            os.chmod(config_path, 0o600)
            process = subprocess.run(
                ["curl", "--config", str(config_path)],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False,
            )
            if process.returncode != 0:
                preview = (
                    output_path.read_text(errors="replace")[:200]
                    if output_path.exists()
                    else ""
                )
                raise RuntimeError(
                    f"curl failed ({process.returncode}): "
                    f"{process.stderr.strip()} body={preview!r}"
                )
            header_lines = headers_path.read_text(encoding="iso-8859-1").splitlines()
            status_lines = [line for line in header_lines if line.startswith("HTTP/")]
            status = status_lines[-1] if status_lines else "missing"
            location = None
            for line in header_lines:
                name, separator, value = line.partition(":")
                if separator and name.lower() == "location":
                    location = value.strip()
            if location:
                return {}, status, location
            raw = output_path.read_bytes()
            if len(raw) > byte_cap:
                raise RuntimeError("response exceeded the local byte cap")
            try:
                value = json.loads(raw)
            except json.JSONDecodeError as error:
                raise RuntimeError(f"invalid JSON ({status}): {error}") from error
            if not isinstance(value, dict):
                raise RuntimeError("top-level JSON response is not an object")
            return value, status, None


def fetch_session(
    client: CurlJsonClient,
    start_url: str,
    *,
    byte_cap: int,
) -> tuple[dict[str, Any], str, str]:
    """Fetch Session through bounded same-authority manual redirects."""
    validate_https_url(start_url, context="Session")
    current = start_url
    final_status = "missing"
    for _redirect in range(4):
        session, status, location = client.request(
            current, body=None, byte_cap=byte_cap
        )
        final_status = status
        if location is None:
            return session, current, final_status
        target = urljoin(current, location)
        validate_https_url(target, context="Session redirect")
        if authority(target) != authority(start_url):
            raise RuntimeError(f"unapproved cross-authority Session redirect: {target}")
        current = target
    raise RuntimeError("too many Session redirects")


def select_mail_account(session: dict[str, Any]) -> tuple[str, dict[str, Any]]:
    """Return the primary or first mail-capable Account."""
    accounts = session.get("accounts")
    if not isinstance(accounts, dict):
        raise RuntimeError("Session accounts is not an object")
    primary = session.get("primaryAccounts")
    account_id = primary.get(MAIL_CAPABILITY) if isinstance(primary, dict) else None
    if account_id is None:
        account_id = next(
            (
                candidate_id
                for candidate_id, account in accounts.items()
                if isinstance(account, dict)
                and MAIL_CAPABILITY in account.get("accountCapabilities", {})
            ),
            None,
        )
    if not isinstance(account_id, str) or account_id not in accounts:
        raise RuntimeError("no mail-capable Account")
    account = accounts[account_id]
    if not isinstance(account, dict):
        raise RuntimeError("selected Account is not an object")
    return account_id, account


def run_smoke(arguments: argparse.Namespace) -> None:
    """Run Session discovery and read-only Mail methods."""
    password = read_password(arguments.password_file)
    client = CurlJsonClient(arguments.user, password)
    session, session_url, session_status = fetch_session(
        client, arguments.session_url, byte_cap=arguments.session_byte_cap
    )
    required_session = {
        "capabilities",
        "accounts",
        "primaryAccounts",
        "username",
        "apiUrl",
        "downloadUrl",
        "uploadUrl",
        "eventSourceUrl",
        "state",
    }
    missing = required_session - session.keys()
    if missing:
        raise RuntimeError(f"Session missing keys: {sorted(missing)}")
    api_url = session["apiUrl"]
    if not isinstance(api_url, str):
        raise RuntimeError("Session apiUrl is not a string")
    validate_https_url(api_url, context="API")
    if authority(api_url) != authority(session_url):
        raise RuntimeError(f"API URL needs explicit authority approval: {api_url}")

    account_id, account = select_mail_account(session)
    account_capabilities = account.get("accountCapabilities", {})
    if not isinstance(account_capabilities, dict):
        raise RuntimeError("Account capabilities is not an object")

    method_calls: list[list[Any]] = [
        ["Core/echo", {"probe": "chidu-read-only"}, "echo"],
        [
            "Mailbox/get",
            {
                "accountId": account_id,
                "properties": [
                    "id",
                    "name",
                    "role",
                    "parentId",
                    "sortOrder",
                    "isSubscribed",
                    "myRights",
                    "totalEmails",
                    "unreadEmails",
                    "totalThreads",
                    "unreadThreads",
                ],
            },
            "mailboxes",
        ],
        [
            "Email/get",
            {"accountId": account_id, "ids": [], "properties": ["id"]},
            "email-state",
        ],
        [
            "Email/query",
            {
                "accountId": account_id,
                "filter": None,
                "sort": None,
                "position": 0,
                "limit": 1,
                "calculateTotal": False,
                "collapseThreads": False,
            },
            "email-query",
        ],
    ]
    using = [CORE_CAPABILITY, MAIL_CAPABILITY]
    if SUBMISSION_CAPABILITY in account_capabilities:
        using.append(SUBMISSION_CAPABILITY)
        method_calls.append(
            [
                "Identity/get",
                {
                    "accountId": account_id,
                    "properties": [
                        "id",
                        "name",
                        "email",
                        "replyTo",
                        "bcc",
                        "textSignature",
                        "htmlSignature",
                        "mayDelete",
                    ],
                },
                "identities",
            ]
        )

    response, api_status, location = client.request(
        api_url,
        body={"using": using, "methodCalls": method_calls},
        byte_cap=arguments.api_byte_cap,
    )
    if location is not None:
        raise RuntimeError(f"API redirected unexpectedly: {location}")
    method_responses = response.get("methodResponses")
    if not isinstance(method_responses, list):
        raise RuntimeError("missing methodResponses")
    by_call_id = {
        item[2]: item
        for item in method_responses
        if isinstance(item, list) and len(item) == 3 and isinstance(item[2], str)
    }
    expected_call_ids = {call[2] for call in method_calls}
    if set(by_call_id) != expected_call_ids:
        raise RuntimeError(
            "call-id mismatch: "
            f"expected {sorted(expected_call_ids)}, got {sorted(by_call_id)}"
        )
    method_errors = [
        (
            call_id,
            item[1].get("type", "unknown") if isinstance(item[1], dict) else "unknown",
        )
        for call_id, item in by_call_id.items()
        if item[0] == "error"
    ]
    if method_errors:
        raise RuntimeError(f"JMAP method errors: {method_errors}")

    mailbox_result = by_call_id["mailboxes"][1]
    email_state_result = by_call_id["email-state"][1]
    email_query_result = by_call_id["email-query"][1]
    identity_response = by_call_id.get("identities")
    if not isinstance(mailbox_result, dict) or not isinstance(email_state_result, dict):
        raise RuntimeError("Mail response has an invalid shape")
    if email_state_result.get("list") != [] or email_state_result.get("notFound") != []:
        raise RuntimeError("Email/get ids=[] returned unexpected objects")
    query_ids = email_query_result.get("ids")
    if not isinstance(query_ids, list) or len(query_ids) > 1:
        raise RuntimeError("Email/query limit was not respected")
    identities = identity_response[1].get("list", []) if identity_response else []

    print("session_discovery=PASS")
    print(f"session_status={session_status}")
    print(f"session_redirect_target={session_url}")
    print(f"api_status={api_status}")
    print(f"username_matches={session.get('username') == arguments.user}")
    print(f"core_capability={CORE_CAPABILITY in session.get('capabilities', {})}")
    print(f"mail_capability={MAIL_CAPABILITY in account_capabilities}")
    print(f"submission_capability={SUBMISSION_CAPABILITY in account_capabilities}")
    print(f"account_count={len(session['accounts'])}")
    print(f"mailbox_count={len(mailbox_result.get('list', []))}")
    print(f"identity_count={len(identities)}")
    print(f"email_state_present={bool(email_state_result.get('state'))}")
    print(f"query_state_present={bool(email_query_result.get('queryState'))}")
    print(f"query_returned_one_or_zero={len(query_ids)}")
    print("read_only_jmap_methods=PASS")


def parse_arguments() -> argparse.Namespace:
    """Parse CLI options."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--session-url", required=True)
    parser.add_argument("--user", required=True)
    parser.add_argument("--password-file", required=True, type=Path)
    parser.add_argument("--session-byte-cap", type=int, default=1 * 1024 * 1024)
    parser.add_argument("--api-byte-cap", type=int, default=4 * 1024 * 1024)
    return parser.parse_args()


def main() -> None:
    """CLI entry point."""
    try:
        run_smoke(parse_arguments())
    except (OSError, RuntimeError, ValueError) as error:
        raise SystemExit(f"smoke failed: {error}") from error


if __name__ == "__main__":
    main()
