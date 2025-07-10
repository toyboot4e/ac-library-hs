# verify

Run online judge problems on your local machine with [verification-helper](https://github.com/online-judge-tools/verification-helper).

## Setup

You need to install [`verification-helper`](https://github.com/online-judge-tools/verification-helper), [`fzf`](https://github.com/junegunn/fzf) and [`just`](https://github.com/casey/just).

### `verification-helper` installtaion via `venv`

```sh
$ pwd
/home/tbm/dev/hs/ac-library-hs
$ cd verify/
$ python3 -m venv .venv
$ source .venv/bin/activate        # on bash or zsh
$ # source .venv/bin/activate.fish # on fish shell
(.venv) $ which pip3
/home/tbm/dev/hs/ac-library-hs/verify/.venv/bin/pip3
(.venv) $ pip3 install online-judge-verify-helper
(.venv) $ # You're ready!
```

### `fzf` and `just`

It would be trivial to install `fzf` or `just` with a package manager of your choice.

## How to run

Select one problem and run the solution on your machine:

```sh
$ just verify
```

Run all the problem solutions:

```sh
$ just verify-all
```

