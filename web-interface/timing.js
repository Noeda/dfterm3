dfterm3_timings = function () {
    var clamp = function (x, min, max) {
        if ( x < min ) { return min; }
        if ( x > max ) { return max; }
        return x;
    }
    var fade = function( element
                       , when_to_start_from_now
                       , times
                       , increment
                       , at_end )
        {
            var st = element.style;
            if ( st.opacity === "" ) {
                st.opacity = 1;
            }
            if ( when_to_start_from_now === undefined ) {
                when_to_start_from_now = 0;
            }
            var f = exports.do_after( when_to_start_from_now
                    , function() {
                        element.style.display = "inline";
                        if ( times > 0 ) {
                            var new_opacity = parseFloat(st.opacity) +
                                              increment;
                            st.opacity = clamp(new_opacity, 0.0, 1.0);
                            times -= 1;
                            exports.do_after(0.01, f);
                        } else {
                            at_end();
                        }
                    } )
            return element;
        }
    var exports = { seconds: function(x) { return x; }
           , do_after: function(time, fun) {
                           window.setTimeout( fun, time*1000 );
                           return fun;
                       }
           , fade_in: function(element, when_to_start_from_now) {
                          fade( element, when_to_start_from_now
                              , 100, 0.01
                              , function() { });
                      }
           , fade_out: function(element, when_to_start_from_now) {
                          fade( element, when_to_start_from_now
                              , 100, -0.01, function() {
                                  element.style.display = "none";
                              });
                        } }
    return exports;
}();

