(function() {
    "use strict";

    AR.lazy = function(f) {
        return function() {
            var x = f();
            f = function() { return x; };
            return x;
        };
    };
})();
