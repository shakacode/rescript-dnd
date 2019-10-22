# re-dnd

[![npm version](https://img.shields.io/npm/v/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)
[![build status](https://img.shields.io/travis/alexfedoseev/re-dnd/master.svg?style=flat-square)](https://travis-ci.org/alexfedoseev/re-dnd)
[![license](https://img.shields.io/npm/l/re-dnd.svg?style=flat-square)](https://www.npmjs.com/package/re-dnd)

Drag-n-drop for [`reason-react`](https://reasonml.github.io/reason-react/).

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

## Examples
* Demo: [`live`](https://re-dnd.now.sh) | [`sources`](./examples)
* Production app: [`Minima`](https://minima.app)

## State

<pre align="center">
🚧 === The library is not feature-complete and some features/apis might be missing === 🚧
🚧 ===                     Documentation is lacking as well                        === 🚧
🌎 ===                   * But we're using it in production *                      === 🌎
</pre>

### Features we'd like to add at some point
- [ ] Keyboard interactions
- [ ] Ignore form elements (opt-out)
- [ ] Drop-zones
- [ ] Multi-select

## Thanks
* To [`react-beautiful-dnd`](https://github.com/atlassian/react-beautiful-dnd) for inspiration

## License
MIT.
