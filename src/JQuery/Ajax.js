/* global exports */
"use strict";

exports.jqueryAjaxImpl = function jqueryAjaxImpl(settings, onError, onSuccess) {
  return function() {
    return jQuery.ajax(jQuery.extend({}, settings, {
      success: function(data) {
        onSuccess(data)();
      },
      error: function(error) {
        onError(error)();
      }
    }));
  }
}

exports.unsafeToOption = function unsafeToOption(s) {
  return s;
}
