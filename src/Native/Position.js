Elm.Native = Elm.Native || {};
Elm.Native.Position = Elm.Native.Position || {};

Elm.Native.Position.make = function (localRuntime) {
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Position = localRuntime.Native.Position || {};

  if (!localRuntime.Native.Position.values) {
    var Task = Elm.Native.Task.make(localRuntime);
    var Maybe = Elm.Maybe.make(localRuntime);
    var Tuple2 = Elm.Native.Utils.make(localRuntime).Tuple2;

    var getIdFromPoint = function (xy) {
      var element = document.elementFromPoint(xy._0, xy._1);
      if (element && element.id){
        return Maybe.Just(element.id);
      } else {
        return Maybe.Nothing;
      }
    };

    localRuntime.Native.Position.values = {
      getIdFromPoint: function(xy){
        return Task.asyncFunction(function (callback) {
          callback(Task.succeed(getIdFromPoint(xy)));
        });
      }
    };

  }

  return localRuntime.Native.Position.values;
};
