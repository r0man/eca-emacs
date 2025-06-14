# ECA Emacs

[![MELPA](https://melpa.org/packages/eca-emacs-badge.svg)](https://melpa.org/#/eca-emacs)
[![License: GPL-3.0+](https://img.shields.io/badge/License-GPLv3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)

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

You can install from MELPA with package.el:

```
M-x package-install eca
```

## Quickstart

1. Run `M-x eca` to start the eca process and initialize the workspace.
2. The dedicated chat window `*eca-chat*` pops up.
3. Type your prompt after the `> ` and press RET.
4. Attach more context auto completing after the `@`.

## Configuration

Above are the most relevant configs one may want change:

```elisp
(setq eca-chat-custom-behavior 'auto
      eca-chat-window-width 50
      eca-custom-command '("eca" "server"))
```

## Roadmap

- Auto download native binary.
- Improve docs.
- Multi session/projects support.
- transient support.
- Auto edit files support (agent). (depends on server)
- Code completion (depends on server)

## Contributing

Contributions are very welcome, please open a issue for discussion or pull request.
