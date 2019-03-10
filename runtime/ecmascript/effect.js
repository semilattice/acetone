"use strict";

// TODO: Use an optimized thread scheduler similar to the ones used in
// TODO: purescript-aff and ZIO, instead of the naive implementation presented
// TODO: in this source file.

function AReffectPure(x) {
    return function(success, error) {
        success(x);
    };
};

function AReffectBind(a, k) {
    return function(success, error) {
        a(function(x) {
            k(x)(success, error);
        }, error);
    };
}
