-module(xmerl_dom).
-export([remove/2]).

remove(XPath, Doc) ->
    xmerl_dom_remove:remove(XPath, Doc).
