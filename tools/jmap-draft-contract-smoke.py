#!/usr/bin/env python3
"""Reversible live smoke for Chidu's remote Draft semantic contract.

The smoke creates one uniquely identifiable attachment-backed Draft, fetches the
exact Email/get shape consumed by Chidu, validates it through the production
Elisp observation/projector, and destroys every Draft carrying the unique
Message-ID before exit.  It prints no credentials, object ids, addresses,
headers, or message content.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time
from types import ModuleType
from typing import Any
from urllib.parse import quote
import uuid

CORE_CAPABILITY = "urn:ietf:params:jmap:core"
MAIL_CAPABILITY = "urn:ietf:params:jmap:mail"
SUBMISSION_CAPABILITY = "urn:ietf:params:jmap:submission"


def load_read_only_smoke_module() -> ModuleType:
    """Load the existing bounded curl/JMAP helpers without duplicating policy."""
    path = Path(__file__).with_name("jmap-live-smoke.py")
    spec = importlib.util.spec_from_file_location("chidu_jmap_live_smoke", path)
    if spec is None or spec.loader is None:
        raise RuntimeError("unable to load the read-only JMAP smoke helpers")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def request_json(
    client: Any, url: str, body: dict[str, Any], *, byte_cap: int
) -> dict[str, Any]:
    """Run one non-redirected bounded JMAP JSON request."""
    value, _status, location = client.request(url, body=body, byte_cap=byte_cap)
    if location is not None:
        raise RuntimeError("JMAP API unexpectedly redirected")
    return value


def method_arguments(
    response: dict[str, Any], name: str, call_id: str
) -> dict[str, Any]:
    """Return exact method response arguments for NAME and CALL_ID."""
    responses = response.get("methodResponses")
    if not isinstance(responses, list) or len(responses) != 1:
        raise RuntimeError("JMAP response did not contain exactly one invocation")
    invocation = responses[0]
    if not isinstance(invocation, list) or len(invocation) != 3:
        raise RuntimeError("JMAP response invocation is malformed")
    actual_name, arguments, actual_call_id = invocation
    if actual_name == "error":
        raise RuntimeError("JMAP method returned an error response")
    if (
        actual_name != name
        or actual_call_id != call_id
        or not isinstance(arguments, dict)
    ):
        raise RuntimeError("JMAP response invocation did not match the request")
    return arguments


def expand_upload_url(template: str, account_id: str) -> str:
    """Expand the required level-1 accountId upload template."""
    if (
        template.count("{accountId}") != 1
        or "{" in template.replace("{accountId}", "")
        or "}" in template.replace("{accountId}", "")
    ):
        raise RuntimeError("unsupported uploadUrl template")
    return template.replace("{accountId}", quote(account_id, safe="-._~"))


def upload_bytes(
    *,
    helper: Any,
    url: str,
    user: str,
    password: str,
    payload: bytes,
    max_size_upload: int,
) -> dict[str, Any]:
    """Upload PAYLOAD through a private curl config and return bounded JSON."""
    if len(payload) > max_size_upload:
        raise RuntimeError("smoke payload exceeds maxSizeUpload")
    with tempfile.TemporaryDirectory(prefix="chidu-draft-upload-") as directory:
        root = Path(directory)
        os.chmod(root, 0o700)
        config = root / "curl.conf"
        source = root / "payload"
        output = root / "response"
        source.write_bytes(payload)
        os.chmod(source, 0o600)
        config.write_text(
            "\n".join(
                [
                    f"url = {helper.curl_config_quote(url)}",
                    f"user = {helper.curl_config_quote(f'{user}:{password}')}",
                    "silent",
                    "show-error",
                    "fail-with-body",
                    'proto = "=https"',
                    'proto-redir = "=https"',
                    "location",
                    "max-redirs = 5",
                    "connect-timeout = 10",
                    "max-time = 30",
                    "max-filesize = 1048576",
                    f"output = {helper.curl_config_quote(str(output))}",
                    'request = "POST"',
                    'header = "Accept: application/json"',
                    'header = "Content-Type: text/plain"',
                    f"data-binary = {helper.curl_config_quote(f'@{source}')}",
                ]
            )
            + "\n",
            encoding="utf-8",
        )
        os.chmod(config, 0o600)
        process = subprocess.run(
            ["curl", "--disable", "--config", str(config)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
        )
        if process.returncode != 0:
            raise RuntimeError(
                f"Blob upload failed with curl exit {process.returncode}"
            )
        raw = output.read_bytes()
        if len(raw) > 1024 * 1024:
            raise RuntimeError("Blob upload response exceeded the local byte cap")
        value = json.loads(raw)
        if not isinstance(value, dict):
            raise RuntimeError("Blob upload response is not an object")
        return value


def single_mailbox_id(response: dict[str, Any], role: str) -> str:
    """Return the unique Mailbox id carrying ROLE."""
    arguments = method_arguments(response, "Mailbox/get", "mailboxes")
    mailboxes = arguments.get("list")
    if not isinstance(mailboxes, list):
        raise RuntimeError("Mailbox/get list is malformed")
    matches = [
        item.get("id")
        for item in mailboxes
        if isinstance(item, dict) and item.get("role") == role
    ]
    if len(matches) != 1 or not isinstance(matches[0], str):
        raise RuntimeError(f"expected exactly one {role} Mailbox")
    return matches[0]


def first_identity(response: dict[str, Any]) -> dict[str, str | None]:
    """Return one usable JMAP Identity name/email projection."""
    arguments = method_arguments(response, "Identity/get", "identities")
    identities = arguments.get("list")
    if not isinstance(identities, list):
        raise RuntimeError("Identity/get list is malformed")
    for identity in identities:
        if not isinstance(identity, dict):
            continue
        email = identity.get("email")
        name = identity.get("name")
        if isinstance(email, str) and email:
            return {
                "email": email,
                "name": name if isinstance(name, str) and name else None,
            }
    raise RuntimeError("no usable JMAP Identity")


def query_smoke_draft_ids(
    client: Any,
    api_url: str,
    account_id: str,
    drafts_mailbox_id: str,
    message_id: str,
) -> list[str]:
    """Return bounded Draft ids carrying the unique smoke MESSAGE_ID."""
    response = request_json(
        client,
        api_url,
        {
            "using": [CORE_CAPABILITY, MAIL_CAPABILITY],
            "methodCalls": [
                [
                    "Email/query",
                    {
                        "accountId": account_id,
                        "filter": {
                            "operator": "AND",
                            "conditions": [
                                {"inMailbox": drafts_mailbox_id},
                                {"hasKeyword": "$draft"},
                                {"header": ["Message-ID", message_id]},
                            ],
                        },
                        "collapseThreads": False,
                        "calculateTotal": True,
                        "position": 0,
                        "limit": 8,
                    },
                    "reconcile",
                ]
            ],
        },
        byte_cap=1024 * 1024,
    )
    arguments = method_arguments(response, "Email/query", "reconcile")
    ids = arguments.get("ids")
    total = arguments.get("total")
    if not isinstance(ids, list) or not isinstance(total, int):
        raise RuntimeError("Draft reconciliation response is malformed")
    if (
        total != len(ids)
        or len(ids) > 8
        or not all(isinstance(item, str) for item in ids)
    ):
        raise RuntimeError("Draft reconciliation coverage is incomplete")
    return ids


def destroy_ids(client: Any, api_url: str, account_id: str, ids: list[str]) -> None:
    """Destroy exact IDS and require complete settlement."""
    if not ids:
        return
    response = request_json(
        client,
        api_url,
        {
            "using": [CORE_CAPABILITY, MAIL_CAPABILITY],
            "methodCalls": [
                [
                    "Email/set",
                    {"accountId": account_id, "destroy": ids},
                    "cleanup",
                ]
            ],
        },
        byte_cap=1024 * 1024,
    )
    arguments = method_arguments(response, "Email/set", "cleanup")
    destroyed = arguments.get("destroyed")
    not_destroyed = arguments.get("notDestroyed")
    if sorted(destroyed or []) != sorted(ids) or (not_destroyed not in (None, {})):
        raise RuntimeError("Draft cleanup did not settle every smoke object")


def cleanup_smoke_drafts(
    client: Any,
    api_url: str,
    account_id: str,
    drafts_mailbox_id: str,
    message_id: str,
) -> None:
    """Repeatedly reconcile and remove every unique smoke Draft."""
    last_error: Exception | None = None
    for _attempt in range(4):
        try:
            ids = query_smoke_draft_ids(
                client, api_url, account_id, drafts_mailbox_id, message_id
            )
            if not ids:
                return
            destroy_ids(client, api_url, account_id, ids)
        except Exception as error:  # cleanup must keep retrying on transient loss
            last_error = error
        time.sleep(0.2)
    remaining = query_smoke_draft_ids(
        client, api_url, account_id, drafts_mailbox_id, message_id
    )
    if remaining:
        raise RuntimeError("reversible Draft smoke left remote residue") from last_error


def validate_with_elisp(
    *,
    repository: Path,
    response: dict[str, Any],
    account_id: str,
    remote_email_id: str,
    drafts_mailbox_id: str,
) -> None:
    """Validate RESPONSE through the production Elisp checkout boundary."""
    with tempfile.TemporaryDirectory(prefix="chidu-draft-elisp-") as directory:
        root = Path(directory)
        os.chmod(root, 0o700)
        envelope = root / "envelope.json"
        program = root / "validate.el"
        envelope.write_text(
            json.dumps(
                {
                    "response": response,
                    "accountId": account_id,
                    "emailId": remote_email_id,
                    "draftsMailboxId": drafts_mailbox_id,
                },
                separators=(",", ":"),
            ),
            encoding="utf-8",
        )
        os.chmod(envelope, 0o600)
        program.write_text(
            """;;; lexical-binding: t;\n
(require 'json)
(require 'chidu-jmap-draft-checkout)
(require 'chidu-draft-semantics)
(require 'chidu-result)
(let* ((path (or (getenv \"CHIDU_DRAFT_SMOKE_ENVELOPE\")
                 (error \"missing smoke envelope\")))
       (wire
        (with-temp-buffer
          (insert-file-contents-literally path)
          (json-parse-buffer
           :object-type 'hash-table :array-type 'vector
           :null-object :json-null :false-object :json-false)))
       (response (gethash \"response\" wire))
       (bytes
        (encode-coding-string
         (json-serialize response
                         :null-object :json-null
                         :false-object :json-false)
         'utf-8-unix t))
       (result
        (chidu-jmap-draft-checkout-validate-response
         bytes
         (gethash \"accountId\" wire)
         (gethash \"emailId\" wire)
         (gethash \"draftsMailboxId\" wire))))
  (cond
   ((chidu-draft-editable-snapshot-p result) t)
   ((chidu-result-failure-p result)
    (error \"Draft contract rejected deployed wire: %S\"
           (chidu-result-failure-kind result)))
   (t (error \"Draft contract returned an invalid value\"))))
""",
            encoding="utf-8",
        )
        os.chmod(program, 0o600)
        environment = os.environ.copy()
        environment["CHIDU_DRAFT_SMOKE_ENVELOPE"] = str(envelope)
        process = subprocess.run(
            [
                "eask",
                "exec",
                "emacs",
                "--batch",
                "-Q",
                "-L",
                ".",
                "-l",
                str(program),
            ],
            cwd=repository,
            env=environment,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
        )
        if process.returncode != 0:
            reason = process.stderr.strip().splitlines()[-1:] or ["unknown error"]
            raise RuntimeError(reason[0])


def run_smoke(arguments: argparse.Namespace) -> None:
    """Run one reversible attachment-backed Draft contract smoke."""
    helper = load_read_only_smoke_module()
    password = helper.read_password(arguments.password_file)
    client = helper.CurlJsonClient(arguments.user, password)
    session, _session_url, _status = helper.fetch_session(
        client, arguments.session_url, byte_cap=arguments.session_byte_cap
    )
    api_url = session.get("apiUrl")
    upload_template = session.get("uploadUrl")
    if not isinstance(api_url, str) or not isinstance(upload_template, str):
        raise RuntimeError("Session lacks API/upload URLs")
    helper.validate_https_url(api_url, context="API")
    helper.validate_https_url(
        upload_template.replace("{accountId}", "x"), context="upload"
    )
    account_id, account = helper.select_mail_account(session)
    capabilities = session.get("capabilities")
    if not isinstance(capabilities, dict):
        raise RuntimeError("Session capabilities is malformed")
    core = capabilities.get(CORE_CAPABILITY)
    if not isinstance(core, dict):
        raise RuntimeError("Session lacks the Core capability")
    max_size_upload = core.get("maxSizeUpload")
    if not isinstance(max_size_upload, int) or max_size_upload <= 0:
        raise RuntimeError("Session maxSizeUpload is invalid")
    account_capabilities = account.get("accountCapabilities")
    if (
        not isinstance(account_capabilities, dict)
        or SUBMISSION_CAPABILITY not in account_capabilities
    ):
        raise RuntimeError("Mail Account has no submission/Identity capability")

    mailboxes = request_json(
        client,
        api_url,
        {
            "using": [CORE_CAPABILITY, MAIL_CAPABILITY],
            "methodCalls": [
                [
                    "Mailbox/get",
                    {"accountId": account_id, "properties": ["id", "role"]},
                    "mailboxes",
                ]
            ],
        },
        byte_cap=1024 * 1024,
    )
    drafts_mailbox_id = single_mailbox_id(mailboxes, "drafts")
    identities = request_json(
        client,
        api_url,
        {
            "using": [CORE_CAPABILITY, SUBMISSION_CAPABILITY],
            "methodCalls": [
                [
                    "Identity/get",
                    {"accountId": account_id, "properties": ["id", "name", "email"]},
                    "identities",
                ]
            ],
        },
        byte_cap=1024 * 1024,
    )
    identity = first_identity(identities)
    message_id = f"chidu-draft-contract-{uuid.uuid4().hex}@invalid"
    creation_id = f"draft-{uuid.uuid4().hex}"
    payload = b"Chidu reversible Draft contract smoke.\n"
    upload = upload_bytes(
        helper=helper,
        url=expand_upload_url(upload_template, account_id),
        user=arguments.user,
        password=password,
        payload=payload,
        max_size_upload=max_size_upload,
    )
    if (
        upload.get("accountId") != account_id
        or not isinstance(upload.get("blobId"), str)
        or upload.get("size") != len(payload)
        or upload.get("type") != "text/plain"
    ):
        raise RuntimeError("Blob upload response did not preserve exact evidence")
    blob_id = upload["blobId"]

    try:
        create = request_json(
            client,
            api_url,
            {
                "using": [CORE_CAPABILITY, MAIL_CAPABILITY],
                "methodCalls": [
                    [
                        "Email/set",
                        {
                            "accountId": account_id,
                            "create": {
                                creation_id: {
                                    "mailboxIds": {drafts_mailbox_id: True},
                                    "keywords": {"$draft": True, "$seen": True},
                                    "from": [identity],
                                    "messageId": [message_id],
                                    "subject": "Chidu Draft contract smoke",
                                    "bodyStructure": {
                                        "type": "multipart/mixed",
                                        "subParts": [
                                            {"partId": "text", "type": "text/plain"},
                                            {
                                                "blobId": blob_id,
                                                "type": "text/plain",
                                                "name": "contract-smoke.txt",
                                                "charset": "utf-8",
                                                "disposition": "attachment",
                                                "language": ["en"],
                                            },
                                        ],
                                    },
                                    "bodyValues": {
                                        "text": {
                                            "value": "Chidu Draft contract smoke body."
                                        }
                                    },
                                }
                            },
                        },
                        "create",
                    ]
                ],
            },
            byte_cap=1024 * 1024,
        )
        create_arguments = method_arguments(create, "Email/set", "create")
        created = create_arguments.get("created")
        not_created = create_arguments.get("notCreated")
        if not isinstance(created, dict) or (not_created not in (None, {})):
            raise RuntimeError("Email/set did not create the smoke Draft")
        created_value = created.get(creation_id)
        if not isinstance(created_value, dict) or not isinstance(
            created_value.get("id"), str
        ):
            raise RuntimeError("Email/set omitted the created Draft id")
        remote_email_id = created_value["id"]

        fetch = request_json(
            client,
            api_url,
            {
                "using": [CORE_CAPABILITY, MAIL_CAPABILITY],
                "methodCalls": [
                    [
                        "Email/get",
                        {
                            "accountId": account_id,
                            "ids": [remote_email_id],
                            "properties": [
                                "id",
                                "blobId",
                                "mailboxIds",
                                "keywords",
                                "headers",
                                "messageId",
                                "inReplyTo",
                                "references",
                                "from",
                                "sender",
                                "subject",
                                "bodyStructure",
                                "bodyValues",
                                "textBody",
                                "htmlBody",
                                "attachments",
                            ],
                            "bodyProperties": [
                                "partId",
                                "blobId",
                                "size",
                                "name",
                                "type",
                                "charset",
                                "disposition",
                                "cid",
                                "language",
                                "location",
                                "headers",
                                "subParts",
                            ],
                            "fetchTextBodyValues": True,
                            "fetchHTMLBodyValues": True,
                            "maxBodyValueBytes": 0,
                        },
                        "draft-checkout",
                    ]
                ],
            },
            byte_cap=8 * 1024 * 1024,
        )
        validate_with_elisp(
            repository=Path(__file__).resolve().parent.parent,
            response=fetch,
            account_id=account_id,
            remote_email_id=remote_email_id,
            drafts_mailbox_id=drafts_mailbox_id,
        )
    finally:
        cleanup_smoke_drafts(client, api_url, account_id, drafts_mailbox_id, message_id)


def parse_arguments() -> argparse.Namespace:
    """Parse bounded smoke configuration."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--session-url", required=True)
    parser.add_argument("--user", required=True)
    parser.add_argument("--password-file", required=True, type=Path)
    parser.add_argument("--session-byte-cap", type=int, default=2 * 1024 * 1024)
    return parser.parse_args()


def main() -> int:
    """Run the smoke without exposing sensitive values."""
    try:
        run_smoke(parse_arguments())
    except Exception as error:
        print(f"draft-contract-smoke: failed: {error}")
        return 1
    print("draft-contract-smoke: passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
