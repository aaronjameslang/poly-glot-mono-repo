# Javascript Modules

## CommonJS (CJS)

CommonJS modules were the only supported style of modules in NodeJS up until version 12

```js
const { someImport } = require("some-module");

module.exports = {
  someExport: {},
};
```

You can mark your file as a CommonJS module by either naming it with the .cjs extension, or by using the type: "commonjs" in your package.json.

CJS was a node standard.

CommonJS does not support importing ESM via the require() method. This is mainly because of its synchronous nature. But you can always just use await import() instead.

## EcmaScript Modules (ESM)

ESM got standardized later and are the only natively supported module style in browsers

ESM is a Ecmascript/Javascript standard, which arrived later but effectively replaces CJS.

```js
import { someImport } from "some-module";

const { someOtherImport } = await import("some-other-module");

export const someExport = {};
```

Also dynamic imports via await import() are now asynchronous

## AMD, UMD

Not used much anymore

AMD looks like Angular v1
```js
define(['dep1', 'dep2'], function (dep1, dep2) {
    //Define the module value by returning a value.
    return function () {};
});
```

UMD is kind of a compatibity layer with the other three
```js
(function (root, factory) {
    if (typeof define === "function" && define.amd) {
        define(["jquery", "underscore"], factory);
    } else if (typeof exports === "object") {
        module.exports = factory(require("jquery"), require("underscore"));
    } else {
        root.Requester = factory(root.$, root._);
    }
}(this, function ($, _) {
    // this is where I defined my module implementation

    var Requester = { // ... };

    return Requester;
}));
```



## Summary

Write in ESM, transpile to CJS and release both if publishing.

## See also

https://nodejs.org/dist/latest/docs/api/esm.html#esm_differences_between_es_modules_and_commonjs


## TODO

Try writing an ESM and a CJS module that imports chai and mocha (which are ESM and CJS module respectively)