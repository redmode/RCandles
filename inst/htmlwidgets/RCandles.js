

HTMLWidgets.widget({

  name: 'RCandles',

  type: 'output',

  initialize: function(el, width, height) {

    // return it as part of our instance data
    return {
      rcandles: rcandles
    };

  },

  renderValue: function(el, x, instance) {

    //inchlib.read_data(x.jsondata);
    //inchlib.draw();

  },

  resize: function(el, width, height, instance) {

  }

});
