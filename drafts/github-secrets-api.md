--------------------------------------------------------------------------------
title: Writing GitHub Secrets to a Repository You Don't Own
published: 2020-05-04
tags: ci, programming
--------------------------------------------------------------------------------

I've been having a lot of fun migrating the CI systems of my repositories to
use GitHub Actions, but it's been more difficult to do the same with projects
that are owned by someone else because I don't have access to the repository
settings that would allow me to create secrets. This means that I can build and
test those projects but not e.g. upload a Docker container as part of
a successful build or upload artifacts somewhere else.

I've tried to work around this limitation by creating a separate repository
that I own and using the `cron` functionality to do this on a schedule, but
this is a poor substitute. I've been frustrated by this situation for a while,
and while reading the
[documentation](https://help.github.com/en/actions/configuring-and-managing-workflows/creating-and-storing-encrypted-secrets)
I noticed this interesting snippet:

> If you are using the REST API to create secrets, anyone with write access to
the repository can create secrets. For more information, see ["GitHub Actions
secrets
API"](https://developer.github.com/v3/actions/secrets/#create-or-update-a-secret-for-a-repository)
in the GitHub Developer documentation.

Amazing! This makes it sound like it's purely a UI issue. So emboldened, I was
able to create and use my secrets only a couple of hours later by poking at the
GitHub API.

I'm going to go ahead and write down the steps I took in order to make this
happen, because this seems like the kind of thing I might have to do more than
once and it's just fiddly enough that I will quickly forget if I don't.

The first thing I need is a GitHub Personal Access Token with the `repo` scope,
which I can create from [this page](https://github.com/settings/tokens).

The next thing to do is to retrieve the public key for the relevant repository:

```bash
$ curl -H "Authorization: token $TOKEN" https://api.github.com/repos/gibiansky/IHaskell/actions/secrets/public-key
{
  "key_id": "$KEY_ID",
  "key": "$PUBLIC_KEY"
}
```

Then I can see what secrets are available:

```bash
$ curl -H "Authorization: token $TOKEN" https://api.github.com/repos/gibiansky/IHaskell/actions/secrets
{
  "total_count": 0,
  "secrets": [
  ]
}
```

The secrets need to be encrypted, and there is sample code for doing this in
Python:

```python
from base64 import b64encode
from nacl import encoding, public

def encrypt(public_key: str, secret_value: str) -> str:
    """Encrypt a Unicode string using the public key."""
    public_key = public.PublicKey(public_key.encode("utf-8"), encoding.Base64Encoder())
    sealed_box = public.SealedBox(public_key)
    encrypted = sealed_box.encrypt(secret_value.encode("utf-8"))
    return b64encode(encrypted).decode("utf-8")
```

I added a Nix shebang line and decided to generate all the encrypted secrets
I needed:

*secret.py*
```python
#! /usr/bin/env nix-shell
#! nix-shell -i python
#! nix-shell -p "python3.withPackages (p: [ p.pynacl ])"

from base64 import b64encode
from nacl import encoding, public

def encrypt(public_key: str, secret_value: str) -> str:
    """Encrypt a Unicode string using the public key."""
    public_key = public.PublicKey(public_key.encode("utf-8"), encoding.Base64Encoder())
    sealed_box = public.SealedBox(public_key)
    encrypted = sealed_box.encrypt(secret_value.encode("utf-8"))
    return b64encode(encrypted).decode("utf-8")

public_key = "$PUBLIC_KEY"

print("CACHIX_SIGNING_KEY=", encrypt(public_key, "$CACHIX_SIGNING_KEY"))
print("DOCKER_USERNAME=", encrypt(public_key, '$DOCKER_USERNAME'))
print("DOCKER_PASSWORD=", encrypt(public_key, '$DOCKER_PASSWORD'))
```

This was easy to run:

```bash
$ chmod +x secret.py
$ ./secret.py
CACHIX_SIGNING_KEY= $ENCRYPTED_CACHIX_SIGNING_KEY
DOCKER_USERNAME= $ENCRYPTED_DOCKER_USERNAME
DOCKER_PASSWORD= $ENCRYPTED_DOCKER_PASSWORD
```

And I chose to update the secrets manually with `curl` even though I could have
automated it with `requests` or something similar (which I might if I have to
do this again soon), for example:

```bash
$ curl -X PUT -H "Authorization: token $TOKEN" -H "Content-Type: application/json" -i https://api.github.com/repos/gibiansky/IHaskell/actions/secrets/CACHIX_SIGNING_KEY -d '{"key_id": "$KEY_ID", "encrypted_value": "$ENCRYPTED_CACHIX_SIGNING_KEY"}'
```

Finally I can check that the secrets were created correctly:

```bash
$ curl -H "Authorization: token $TOKEN" https://api.github.com/repos/gibiansky/IHaskell/actions/secrets
{
  "total_count": 3,
  "secrets": [
    {
      "name": "CACHIX_SIGNING_KEY",
      "created_at": "2020-05-03T04:45:07Z",
      "updated_at": "2020-05-03T04:45:07Z"
    },
    {
      "name": "DOCKER_PASSWORD",
      "created_at": "2020-05-03T04:49:59Z",
      "updated_at": "2020-05-03T04:49:59Z"
    },
    {
      "name": "DOCKER_USERNAME",
      "created_at": "2020-05-03T04:48:52Z",
      "updated_at": "2020-05-03T04:48:52Z"
    }
  ]
}
```

I hope these instructions are useful, future me!
