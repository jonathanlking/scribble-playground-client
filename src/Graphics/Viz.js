"use strict";

exports._viz = function(data, format) {
  var v = undefined;
  if (typeof window === "undefined") {
    v = require('viz.js');
  } else {
    v = Viz;
  }
  return v(data, {format: format})
}
