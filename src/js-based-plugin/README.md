# JS based plugin / unplugin-ezno

A plugin to use Ezno in systems like Vite and webpack. See the [unplugin repo for details](https://github.com/unjs/unplugin)

For example in Vite, `vite.config.js` should look like:

```javascript
import Ezno from 'unplugin-ezno-plugin'

export default {
    plugins: [
        Ezno.vite(),
    ]
}
```

Note `.vite()` to specify using in the Vite build system.
