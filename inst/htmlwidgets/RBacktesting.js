

HTMLWidgets.widget({

  name: 'RBacktesting',

  type: 'output',

  initialize: function(el, width, height) {

    // return it as part of our instance data
    return {
      rbacktesting: rbacktesting
    };

  },

  renderValue: function(el, x, instance) {

  },

  resize: function(el, width, height, instance) {

  }

});
