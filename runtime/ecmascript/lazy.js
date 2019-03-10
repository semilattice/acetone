"use strict";

function ARlazy(f) {
    return function() {
        var x = f();
        f = function() { return x; };
        return x;
    };
}
