# rescript-dnd

[![npm version](https://img.shields.io/npm/v/rescript-dnd.svg?style=flat-square)](https://www.npmjs.com/package/rescript-dnd)
[![license](https://img.shields.io/npm/l/rescript-dnd.svg?style=flat-square)](https://www.npmjs.com/package/rescript-dnd)
[![build](https://github.com/shakacode/re-dnd/actions/workflows/pr.yml/badge.svg)](https://github.com/shakacode/re-dnd/actions/workflows/pr.yml)

Drag & drop for [`@rescript/react`](https://reasonml.github.io/reason-react/).

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
yarn add rescript-dnd bs-webapi
# or npm
npm install --save rescript-dnd bs-webapi
```

Then add it to `bsconfig.json`:

```json
"bs-dependencies": [
  "bs-webapi",
  "rescript-dnd"
]
```

## Docs
* [Getting Started](./docs/01-GettingStartedGuide.md)
* [Advanced guide: Safer identifiers and multiple containers](./docs/02-SaferIdentifiersAndMultipleContainersGuide.md)
* [Api](./docs/03-Api.md)

## Examples
* Demos: [`live`](https://rescript-dnd.vercel.app) | [`sources`](./examples)
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
