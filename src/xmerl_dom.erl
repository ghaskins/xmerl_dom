-module(xmerl_dom).
-include_lib("xmerl/include/xmerl.hrl").
-export([remove/2, process/3]).

remove(XPath, Doc) ->
    xmerl_dom_remove:remove(XPath, Doc).

process(Target, E, Fun) when Target =:= E ->
    Fun(E);
process(_Target, E=#xmlElement{content=[]}, _Fun) ->
    [E];
process(Target, E, Fun) when is_record(E, xmlElement) ->
    Content = E#xmlElement.content,
    Parent = self(),
    Work = fun(SubE) ->
		   Parent ! {self(), process(Target, SubE, Fun)}
	   end,
    Pids = [spawn(fun() -> Work(SubE) end) || SubE <- Content],
    New = lists:foldl(fun(Pid, Acc) ->
			       receive
				   {Pid, Val} -> lists:flatten(Acc, Val)
			       end
		     end,
		     [],
		     Pids),
    [E#xmlElement{content=New}];
process(_Target, E, _Fun) ->
    [E].

