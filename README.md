# re-dnd

[![npm version](https://img.shields.io/npm/v/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)
[![build status](https://img.shields.io/travis/alexfedoseev/re-dnd/master.svg?style=flat-square)](https://travis-ci.org/alexfedoseev/re-dnd)
[![license](https://img.shields.io/npm/l/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)

Drag & drop for [`@rescript/react`](https://reasonml.github.io/reason-react/).

## NOTE

This is a forked version of re-dnd.

At Plow, we use Nix to manage our dependencies and setting up Nix to build PPX can be a lot of work (plus the maintenance).
This forked version drops `rescript-logger` and switches `bs-platform` with `rescript`.


## Features
* Vertical lists
* Horizontal lists
* Multiple drop targets
* Mouse & Touch interactions
* Conditional drag & drop
* Custom drag handles
* Scrollable containers
* Auto-scroll when dragging at container's edge

## Installation

```shell
# yarn
yarn add re-dnd
# or npm
npm install --save re-dnd
```

Then add it to `bsconfig.json`:

```json
"bs-dependencies": [
  "re-dnd"
]
```

## Docs
* [Getting Started](./docs/01-GettingStartedGuide.md)
* [Advanced guide: Safer identifiers and multiple containers](./docs/02-SaferIdentifiersAndMultipleContainersGuide.md)
* [Api](./docs/03-Api.md)

## Examples
* Demos: [`live`](https://re-dnd.now.sh) | [`sources`](./examples)
* Production app: [`Minima`](https://minima.app)

## State
🚧 The library is not feature-complete and some features/apis might be missing.<br>
🎙 Let us know if you miss anything via [creating an issue](issues/new).<br>
🌎 We're using it in production BTW.


### Features we'd like to add at some point
- [ ] Keyboard interactions
- [ ] Ignore form elements (opt-out)
- [ ] Drop-zones
- [ ] Multi-select

## Thanks
* To [`react-beautiful-dnd`](https://github.com/atlassian/react-beautiful-dnd) for inspiration

## License
MIT.
