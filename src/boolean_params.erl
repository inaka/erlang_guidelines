-module(boolean_params).

-export([bad/1, good/1]).

bad(EdgeLength) -> bad_draw_square(EdgeLength, true).

bad_draw_square(EdgeLength, true) ->
  square:fill(square:draw(EdgeLength));
bad_draw_square(EdgeLength, false) ->
  square:draw(EdgeLength).

good(EdgeLength) -> good_draw_square(EdgeLength, full).

good_draw_square(EdgeLength, full) ->
  square:fill(square:draw(EdgeLength));
good_draw_square(EdgeLength, empty) ->
  square:draw(EdgeLength).
