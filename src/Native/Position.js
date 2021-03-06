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

    /**
     * Convert viewport coordicates (usually propagated by event.pageX and event.pageY etc.)
     * to coordicates in an SVG.
     * Ref:
     * - http://stackoverflow.com/questions/10298658/mouse-position-inside-autoscaled-svg
     * - https://msdn.microsoft.com/ja-jp/library/hh535760(v=vs.85).aspx
     */
    var convertViewportPointToSvgPoint = function (xy) {
      var svg = document.querySelector("svg");
      var point = svg.createSVGPoint();
      point.x = xy._0;
      point.y = xy._1;
      var convertedPoint = point.matrixTransform(svg.getScreenCTM().inverse());
      return Tuple2(convertedPoint.x, convertedPoint.y);
    };

    localRuntime.Native.Position.values = {
      getIdFromPoint: function(xy){
        return Task.asyncFunction(function (callback) {
          callback(Task.succeed(getIdFromPoint(xy)));
        });
      },
      convertViewportPointToSvgPoint: function(xy){
        return Task.asyncFunction(function (callback) {
          callback(Task.succeed(convertViewportPointToSvgPoint(xy)));
        });
      }
    };

  }

  return localRuntime.Native.Position.values;
};
