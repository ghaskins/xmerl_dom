-module(xmerl_dom_remove).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(xmerl_xs, [select/2]).

-export([remove/2]).

remove_i(Target, E) when Target =:= E ->
    drop;
remove_i(Target, E=#xmlElement{content=[]}) ->
    E;
remove_i(Target, E) when is_record(E, xmlElement) ->
    Content = E#xmlElement.content,
    Parent = self(),
    Work = fun(SubE) ->
		   Parent ! {self(), remove_i(Target, SubE)}
	   end,
    Pids = [spawn(fun() -> Work(SubE) end) || SubE <- Content],
    New = lists:foldl(fun(Pid, Acc) ->
			       receive
				   {Pid, drop} -> Acc;
				   {Pid, Val} -> Acc ++ [Val]
			       end
		     end,
		     [],
		     Pids),
    E#xmlElement{content=New};
remove_i(Target, E) ->
    E.

remove(XPath, Doc) ->
    Targets = select(XPath, Doc),
    F = fun(Target, Doc) ->
		remove_i(Target, Doc)
    	end,
    lists:foldl(F, Doc, Targets).

%--------------------------
% unit tests

sample_input() ->
    lists:flatten([
		   "<?xml version=\"1.0\"?>",
		   "<foo>",
		   "<bar id=\"1\">",
		   "<foobar/>",
		   "</bar>",
		   "<bar id=\"2\">",
		   "<foobar/>",
		   "<foobar/>",
		   "</bar>",
		   "<foobar/>",
		   "</foo>"
		  ]
		 ).

expected_output1() ->
    lists:flatten([
		   "<?xml version=\"1.0\"?>",
		   "<foo>",
		   "<bar id=\"2\">",
		   "<foobar/>",
		   "<foobar/>",
		   "</bar>",
		   "<foobar/>",
		   "</foo>"
		  ]
		 ).

expected_output2() ->
    lists:flatten([
		   "<?xml version=\"1.0\"?>",
		   "<foo>",
		   "<bar id=\"1\"/>",
		   "<bar id=\"2\"/>",
		   "</foo>"
		  ]
		 ).

execute(XPath, Input, ExpectedOutput) ->
    ActualXml = xmerl:export_element(remove(XPath, Input), xmerl_xml),
    ExpectedXml = xmerl:export_element(ExpectedOutput, xmerl_xml),
    
    case string:equal(ActualXml, ExpectedXml) of
	true -> ok;
	false ->
	    ?debugFmt("Unexpected resposne from remove():~n Actual: ~s~nExpected: ~s~n",
		      [ActualXml, ExpectedXml]),
	    throw(badresposne)
    end.

remove_test() ->
    {Input, _}
	= xmerl_scan:string(sample_input()),
    {ExpectedOutput1, _}
	= xmerl_scan:string(expected_output1()),
    {ExpectedOutput2, _}
	= xmerl_scan:string(expected_output2()),

    execute("/foo/bar[@id=\"1\"]", Input, ExpectedOutput1),
    execute("//foobar", Input, ExpectedOutput2).


