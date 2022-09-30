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

> ### ShakaCode
> If you are looking for help with the development and optimization of your project, [ShakaCode](https://www.shakacode.com) can help you to take the reliability and performance of your app to the next level.
>
> If you are a developer interested in working on ReScript / TypeScript / Rust / Ruby on Rails projects, [we're hiring](https://www.shakacode.com/career/)!

## Installation

```shell
# yarn
yarn add rescript-dnd rescript-webapi
# or npm
npm install --save rescript-dnd rescript-webapi
```

Then add it to `bsconfig.json`:

```json
"bs-dependencies": [
  "rescript-dnd",
  "rescript-webapi"
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

## Supporters

<a href="https://www.jetbrains.com">
  <img src="https://user-images.githubusercontent.com/4244251/184881139-42e4076b-024b-4b30-8c60-c3cd0e758c0a.png" alt="JetBrains" height="120px">
</a>
<a href="https://scoutapp.com">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://user-images.githubusercontent.com/4244251/184881147-0d077438-3978-40da-ace9-4f650d2efe2e.png">
    <source media="(prefers-color-scheme: light)" srcset="https://user-images.githubusercontent.com/4244251/184881152-9f2d8fba-88ac-4ba6-873b-22387f8711c5.png">
    <img alt="ScoutAPM" src="https://user-images.githubusercontent.com/4244251/184881152-9f2d8fba-88ac-4ba6-873b-22387f8711c5.png" height="120px">
  </picture>
</a>
<br />
<a href="https://www.browserstack.com">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://user-images.githubusercontent.com/4244251/184881122-407dcc29-df78-4b20-a9ad-f597b56f6cdb.png">
    <source media="(prefers-color-scheme: light)" srcset="https://user-images.githubusercontent.com/4244251/184881129-e1edf4b7-3ae1-4ea8-9e6d-3595cf01609e.png">
    <img alt="BrowserStack" src="https://user-images.githubusercontent.com/4244251/184881129-e1edf4b7-3ae1-4ea8-9e6d-3595cf01609e.png" height="55px">
  </picture>
</a>
<a href="https://railsautoscale.com">
  <img src="https://user-images.githubusercontent.com/4244251/184881144-95c2c25c-9879-4069-864d-4e67d6ed39d2.png" alt="Rails Autoscale" height="55px">
</a>
<a href="https://www.honeybadger.io">
  <img src="https://user-images.githubusercontent.com/4244251/184881133-79ee9c3c-8165-4852-958e-31687b9536f4.png" alt="Honeybadger" height="55px">
</a>

<br />
<br />

The following companies support our open source projects, and ShakaCode uses their products!
