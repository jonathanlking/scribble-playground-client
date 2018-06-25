// Taken from https://gist.github.com/prathje/7422e49b7c809fe8236bb2f213e7076e
// module Halogen.Component.Raw
//

exports.setHTML = function(el) {
    return function (html) {
        return function() {
            el.innerHTML = html;
        };
    };
};
