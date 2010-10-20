-module(xmerl_dom_remove).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(xmerl_xs, [select/2]).

-export([remove/2]).

-record(acc, {depth="/", element=#xmlElement{}}).

clone(Depth, Element) ->
    #acc{depth=Depth, element=Element#xmlElement{content=[]}}.

depth(Current, Acc) ->
    lists:flatten(io_lib:format("~s~s/",
				[Acc#acc.depth, Current#xmlElement.name])).

push(Acc, Content) ->
    Element = Acc#acc.element,
    NewContent = lists:append(Element#xmlElement.content, [Content]),
    Acc#acc{element=Element#xmlElement{content=NewContent}}.

remove(Target, [Current | T], Acc) when Target =:= Current ->
    remove(Target, T, Acc);
remove(Target, [Current=#xmlElement{content=[]} | T], Acc) ->
    remove(Target, T, push(Acc, Current)); 
remove(Target, [Current | T], Acc) when is_record(Current, xmlElement) ->
    Depth = depth(Current, Acc),
    Content = Current#xmlElement.content,
    New = remove(Target, Content, clone(Depth, Current)),
    remove(Target, T, push(Acc, New));
remove(Target, [Current | T], Acc) ->
    remove(Target, T, push(Acc, Current));
remove(Target, [], Acc) ->
    Acc#acc.element.

remove(XPath, Doc) ->
    Targets = select(XPath, Doc),
    F = fun(Target, Doc) ->
		#xmlElement{content=[NewDoc]} =
		    remove(Target, [Doc], clone("/", #xmlElement{content=Doc})),
    
		NewDoc
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


