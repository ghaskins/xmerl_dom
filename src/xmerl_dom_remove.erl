-module(xmerl_dom_remove).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([execute/3]).

execute(XPath, Doc, []) ->
    Targets = xmerl_xs:select(XPath, Doc),
    F = fun(Target, Doc) ->
		[NewDoc] = xmerl_dom:select(Target, Doc, fun(E) -> [] end),
		NewDoc
    	end,
    lists:foldl(F, Doc, Targets);
execute(_XPath, _Doc, Options) ->
    throw({badarg, Options}).

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

run(XPath, Input, ExpectedOutput) ->
    ActualOutput = xmerl_dom:remove(XPath, Input, []),

    % normalize the XMerL structures to standard XML for comparison.
    % 
    % We could try to compare Actual =:= Expected directly, but the
    % problem is that there might be slight meta-data differences
    % in the record fields that do not affect equality at the XML
    % level
    ActualXml = xmerl:export_element(ActualOutput, xmerl_xml),
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

    run("/foo/bar[@id=\"1\"]", Input, ExpectedOutput1),
    run("//foobar", Input, ExpectedOutput2).

badarg_test() ->
    try xmerl_dom:remove('_', '_', [badoption]) of
	_ -> throw(validation_failure)
    catch
	throw:{badarg, [badoption]} -> ok
    end.


