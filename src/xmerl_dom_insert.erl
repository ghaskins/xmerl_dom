-module(xmerl_dom_insert).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([execute/4]).

validate_options([{position, insert_before} | T]) ->
    validate_options(T);
validate_options([{position, insert_after} | T]) ->
    validate_options(T);
validate_options([{position, insert_under} | T]) ->
    validate_options(T);
validate_options([Option | _T]) ->
    throw({badoption, Option});
validate_options([]) ->
    ok.

visitor(E1, insert_before, E2) ->
    [E1, E2];
visitor(E1, insert_after, E2) ->
    [E2, E1];
visitor(E1, insert_under, E2) ->
    [E2#xmlElement{content=lists:append(E2#xmlElement.content, [E1])}].

execute(E, XPath, Doc, Options) ->
    validate_options(Options),
    OptionDict = dict:from_list(Options),

    % inserts may only have one matching node
    Target = case xmerl_xs:select(XPath, Doc) of
		 [] -> throw(no_matches);
		 [H | []] -> H;
		 R=[H | T] -> throw({ambiguous_match, R})
	     end,

    Mode = case dict:find(position, OptionDict) of
	       {ok, Value} -> Value;
	       _ -> insert_after
	   end,

    [NewDoc] = xmerl_dom:select(Target, Doc,
				fun(E2) -> visitor(E, Mode, E2) end),
    
    NewDoc.

%--------------------------
% unit tests

sample_input() ->
    lists:flatten([
		   "<?xml version=\"1.0\"?>",
		   "<foo>",
		   "</foo>"
		  ]
		 ).

expected_output() ->
    lists:flatten([
		   "<?xml version=\"1.0\"?>",
		   "<foo>",
		   "<bar id=\"1\">",
		   "</bar>",
		   "<bar id=\"2\">",
		   "<foobar/>",
		   "<foobar/>",
		   "</bar>",
		   "<foobar/>",
		   "</foo>"
		  ]
		 ).

rules() ->
    [
     {"/foo", insert_under,
      #xmlElement{name=bar,
		  attributes=
		      [
		       #xmlAttribute{name=id, value="2"}
		      ]
		 }
     },
     {"/foo/bar[@id=\"2\"]", insert_before,
      #xmlElement{name=bar,
		  attributes=
		      [
		       #xmlAttribute{name=id, value="1"}
		      ]
		 }
     },
     {"/foo/bar[@id=\"2\"]", insert_under,
      #xmlElement{name=foobar}
     },
     {"/foo/bar[@id=\"2\"]/foobar", insert_after,
      #xmlElement{name=foobar}
     },
     {"/foo/bar[@id=\"2\"]", insert_after,
      #xmlElement{name=foobar}
     }
    ].

insert_test() ->
    {Input, _}
	= xmerl_scan:string(sample_input()),
    {ExpectedOutput, _}
	= xmerl_scan:string(expected_output()),

    F = fun({XPath, Mode, E}=Rule, Acc) ->
		xmerl_dom:insert(E, XPath, Acc, [{position, Mode}])
	end,

    ActualOutput = lists:foldl(F, Input, rules()),

    ActualXml = xmerl:export_element(ActualOutput, xmerl_xml),
    ExpectedXml = xmerl:export_element(ExpectedOutput, xmerl_xml),
    
    case string:equal(ActualXml, ExpectedXml) of
	true -> ok;
	false ->
	    ?debugFmt("Unexpected resposne from insert():~n Actual: ~s~nExpected: ~s~n",
		      [ActualXml, ExpectedXml]),
	    throw(badresposne)
    end.



