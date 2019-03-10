(function() {
    "use strict";

    // TODO: Use an optimized thread scheduler similar to the ones used in
    // TODO: purescript-aff and ZIO, instead of the naive implementation
    // TODO: presented in this source file.

    AR.effectPure = function(x) {
        return function(success, error) {
            success(x);
        };
    };

    AR.effectBind = function(a, k) {
        return function(success, error) {
            a(function(x) {
                k(x)(success, error);
            }, error);
        };
    };
})();
