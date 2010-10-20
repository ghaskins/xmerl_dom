-module(xmerl_dom).
-include_lib("xmerl/include/xmerl.hrl").
-export([remove/3, select/3]).

remove(XPath, Doc, Options) ->
    xmerl_dom_remove:execute(XPath, Doc, Options).

select(Target, E, Fun) when Target =:= E ->
    Fun(E);
select(_Target, E=#xmlElement{content=[]}, _Fun) ->
    [E];
select(Target, E, Fun) when is_record(E, xmlElement) ->
    Content = E#xmlElement.content,
    Parent = self(),
    Work = fun(SubE) ->
		   Parent ! {self(), select(Target, SubE, Fun)}
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
select(_Target, E, _Fun) ->
    [E].

