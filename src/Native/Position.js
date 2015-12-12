Elm.Native = Elm.Native || {};
Elm.Native.Position = Elm.Native.Position || {};

Elm.Native.Position.make = function (localRuntime) {
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Position = localRuntime.Native.Position || {};

  if (!localRuntime.Native.Position.values) {
    var Task = Elm.Native.Task.make(localRuntime);

    var getIdFromPoint = function (x, y) {
      var element = document.elementFromPoint(x, y);
      if (element){
        return element.id;
      } else {
        return null;
      }
    };


    localRuntime.Native.Position.values = {
      getIdFromPoint: function(x, y){
        return Task.asyncFunction(function (callback) {
          callback(Task.succeed(getIdFromPoint(x, y)));
        });
      }
    };

  };

  return localRuntime.Native.WebAPI.Position.values;
};
