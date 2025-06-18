# ECA Emacs

:warning: The project is still alpha and experimental, with bugs and missing features, but being consistenly improved.

[![MELPA](https://melpa.org/packages/eca-emacs-badge.svg)](https://melpa.org/#/eca-emacs)
[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](./LICENSE)

![demo](./demo.png)

ECA (Editor Code Assistant) is an AI-powered pair-programming client for Emacs.
Inspired by lsp-mode’s JSONRPC handling, it connects to an external `eca` server process to provide interactive chat, code suggestions, context management and more.

For more details about ECA, check [ECA server](https://github.com/eca/eca).

## Requirements

- Emacs 28.1 or later
- External `eca` server binary
  - Place it on your `$PATH` or customize `eca-custom-command`
  - (Planned: automatic download in a future release)

## Installation

### Melpa (wip)

```
M-x package-install eca
```

### Doom Emacs:

```elisp
(package! eca :recipe (:fetcher github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
```

## Quickstart

1. Run `M-x eca` to start the eca process and initialize the workspace.
2. The dedicated chat window `*eca-chat*` pops up.
3. Type your prompt after the `> ` and press RET.
4. Attach more context auto completing after the `@`.

## Roadmap

- Improve docs.
- Error handling
- Improve add context 
- Multi session/projects support.
- transient support.
- Auto edit files support (agent). (depends on server)
- Code completion (depends on server)

## Contributing

Contributions are very welcome, please open a issue for discussion or pull request.
