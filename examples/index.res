switch ReactDOM.querySelector("#app") {
| Some(root) => ReactDOM.Client.createRoot(root)->ReactDOM.Client.Root.render(<App />)
| None => failwith("DOM node `#app` not found")
}
