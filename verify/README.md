# verify

[verification-helper](https://github.com/online-judge-tools/verification-helper) によりシステムテストを実施します。

TODO: Windows サポート

## セットアップ (仮)

```sh
$ cd verify/                                                                                main ⬆ ✖ ✱ ◼
$ python3 -m venv .venv                                                                   main ⬆ ✖ ✱ ◼
$ python3 -m venv .venv                                                                   main ⬆ ✖ ✱ ◼
$ source .venv/bin/activate
$ # source .venv/bin/activate.fish # on fish shell
(.venv) $ which pip3
/home/tbm/dev/hs/acl-hs/verify/.venv/bin/pip3
(.venv) ❯❯❯ pip3 install online-judge-verify-helper
(略)
```

## 方法 (仮)

[`fzf`](https://github.com/junegunn/fzf) を前提とします。

```sh
(.venv) $ pwd
/home/tbm/dev/hs/acl-hs/verify
(.venv) $ export DROPBOX_TOKEN=<TOKEN> # TODO: 詳細に解説する
(.venv) $ ./script/verify
```

