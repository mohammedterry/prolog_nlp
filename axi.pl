%%%%%%%%%%%%%%  DICTIONARY %%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded( 'et.pl' ).			% ETokeniser - for string manipulation - tokenize_line(user,Tokens),  tokens_words(Tokens,Words)
:- ensure_loaded( 'morph.pl' ).			% to convert words into root form so they can be recognised by Wordnet
:- ensure_loaded( 'wn_s.pl' ). 			% WordNet dictionary - s(synset_ID,w_num,‘word’,ss_type,sense_number,tag_count).
:- ensure_loaded( 'words.pl' ).			% additional words not included in Wordnet
%%%%%%%%%%%%%%%%  MAIN  %%%%%%%%%%%%%%%%%%%%%%%%%
main :-
	tokenize_line(user,Tokens), 		% hello world --> w(h,e,l,l,o), w(w,o,r,l,d).
	write('Tokens: '), write(Tokens),nl,
	tag_commas(Tokens,Tokens_mod),		% s(',') --> w(b,r,e,a,k,c,o,m,m,a)
	write('Commas: '), write(Tokens_mod),nl,
	tokens_words(Tokens_mod,Words),		% w(h,e,l,l,o),w(b,r,e,a,k,c,o,m,m,a), w(w,o,r,l,d) --> hello,breakcomma,world
	write('Words: '), write(Words),nl,
	words2roots(Words,Roots),		% i,didnt,know --> i,do,not,know
	write('Roots: '), write(Roots),nl,
	tag_to(Roots,Roots_mod),		% i,want,you,to,love,her --> i,want,you,cause_to,love,her
	write('Tag to: '), write(Roots_mod),nl,
	tag_auxillary(Roots_mod,Roots_mod2),	% i,be,go,... --> i,be_,go...
	write('Tag aux verbs: '), write(Roots_mod2),nl,
	spell_corrector(Roots_mod2,Roots_mod3),	% mybe, you, cn, crct --> maybe,you,can,correct
	write('Autocorrect: '), write(Roots_mod3),nl,
%	scan4compounds(Roots_mod3,Roots_mod4),	% i,really,really,like,bed,time --> i,really-really,like,bed-time
%	write('Compounds: '), write(Roots_mod4),nl,
	parse(Roots_mod3,Parse_tree),
	write('Parse tree: '), write(Parse_tree),nl,nl,
	write_tree(Parse_tree),nl,
	semantic_net(Parse_tree,0).			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_word(W,T) :-	
	word(W,T,_,_),
	!.
lookup_word(W,T) :-
	lookup_word_iterate(W,T,10000),
	!.					%iteratively increase search depth to speed up
lookup_word(W,T) :-
	search_depth(P),
	s(_,_,W,T,1,P).
%------------------------------------------------
lookup_word_iterate(W,T,Start) :-
	not(s(_,_,W,T,_,Start)),		% look up word type in dictionary or exception list (set wordsense = 1 to limit possibilities) % set probability P >= 3 (higher number is more frequently found word)				
    	Next is Start-1,
	Next >= 0,
	retractall(search_depth(_)),
	assertz(search_depth(Next)),
	lookup_word_iterate(W,T,Next).		
%%%%%%%%%%%% PRE-PROCESSING: WORD -> ROOT %%%%%%%
word2root(Word,[Root,not]) :-			%special case: root,not
	morph_atoms([Word],[[Root,not]]),
	!.
word2root(Word,Root) :-				%convert words into root form for easy recognition in dictionary
	morph_atoms([Word],[[Root,_]]),
	lookup_word(Root,v),
	\+ lookup_word(Word,v),			%except special words
	\+ lookup_word(Word,xv),	
	\+ lookup_word(Word,n),	
	!.
word2root(Word,Root) :-				% only apply to verbs and nouns
	morph_atoms([Word],[[Root,_]]),
	lookup_word(Root,n), 
	\+ lookup_word(Word,v),			%except special words
	\+ lookup_word(Word,xv),	
	\+ lookup_word(Word,n),	
	!.
word2root(Word,Word).
%%-----------------------------------------------
words2roots([Word|Rest],[Root,Not|RestRoots]) :- % special case: root,not
	word2root(Word,List),
	is_list(List),
	List = [Root,Not],
	!,
	words2roots(Rest,RestRoots).
words2roots([Det,Word|Rest],[Det,Word|RestRoots]) :-	%do not apply to "the X"
	word(Det,det,_,_),
	!,
	words2roots(Rest,RestRoots).
words2roots([Word|Rest],[Root|RestRoots]) :-
	word2root(Word,Root),
	!,
	words2roots(Rest,RestRoots).
words2roots([],[]).
%%%%%%%%% PRE-PROCESSING: ',' --> 'breakcomma' %%%%
tag_commas([],[]).
tag_commas([s(',')|Rest], [w([b,r,e,a,k,c,o,m,m,a])|Result]) :-
	!,
	tag_commas(Rest,Result).
tag_commas([X|Rest], [X|Result]) :-
	!,
	tag_commas(Rest,Result).
%%%%%%%%% PRE-PROCESSING: 'to+verb' --> 'cause_to' %
tag_to([],[]).
tag_to([to,Verb|Rest], [cause_to,Verb|Result]) :-
	lookup_word(Verb,v),
	!,
	tag_to(Rest,Result).
tag_to([W,to,Verb|Rest], [W,cause_to,Verb|Result]) :-
	lookup_word(Verb,v),
	!,
	tag_to(Rest,Result).
tag_to([W|Rest], [W|Result]) :-
	tag_to(Rest,Result).
%%%%%%%%% PRE-PROCESSING: 'be,help'--> 'be_,help'%% tag auxillary verbs
tag_auxillary([],[]).
tag_auxillary([Aux,not,Verb|Rest], [Aux_,not,Verb|Result]) :-
	lookup_word(Verb,v),
	lookup_word(Aux,v),
	tag(Aux,Aux_),
	!,
	tag_auxillary(Rest,Result).
tag_auxillary([Aux,Intermediate,not,Verb|Rest], [Aux_,Intermediate,not,Verb|Result]) :-
	lookup_word(Verb,v),
	lookup_word(Aux,v),
	tag(Aux,Aux_),
	!,
	tag_auxillary(Rest,Result).
tag_auxillary([Aux,Verb|Rest], [Aux_,Verb|Result]) :-
	lookup_word(Verb,v),
	lookup_word(Aux,v),
	tag(Aux,Aux_),
	!,
	tag_auxillary(Rest,Result).
tag_auxillary([W|Rest], [W|Result]) :-
	tag_auxillary(Rest,Result).
%%%%%%%%% PRE-PROCESSING: W,W-> 'W W' & ADD NAMES %
scan4compounds([W1,Conj,W2|Rest],[W1and2|X]) :- %e.g.   Rosie,and,Jim--> 'Rosie and Jim'
	lookup_word(W1,T), 
	lookup_word(W2,T),
	lookup_word(Conj,conj), 
	join(W1,Conj,W1and),
	join(W1and,W2,W1and2), 
	addWord(W1and2,T), 
	!,
	scan4compounds(Rest,X).
scan4compounds([W1,of,W2|Rest],[W1of2|X]) :- 	%e.g.   spoonful,of,honey --> 'spoonful of honey'
	lookup_word(W1,T), 
	lookup_word(W2,T), 
	join(W1,of,W1of),
	join(W1of,W2,W1of2), 
	addWord(W1of2,T), 
	!,
	scan4compounds(Rest,X).
scan4compounds([W1,W2|Rest],[W12|X]) :- 	%if 2 words next to each other of same type - turn into compound word e.g.   bed,time --> bed time
	lookup_word(W1,T), 
	lookup_word(W2,T),
	\+ word(W1,_,_,subj),
	\+ word(W2,_,_,subj),
	\+ word(W1,_,_,obj),
	\+ word(W2,_,_,obj),
	\+ (lookup_word(W1,v),lookup_word(W2,v)),
	join(W1,W2,W12), 
	addWord(W12,T), 
	!,
	scan4compounds(Rest,X).
scan4compounds([W|Rest],[W|Result]) :-
	!,
	scan4compounds(Rest,Result).
scan4compounds([],[]).
%%-----------------------------------------------
join(A,B,AB) :- 				%joins two words together as one atom
	name(A,Code_a),
	name(B,Code_b), 
	name(' ',Code_space), 
	append(Code_a,Code_space,X), 
	append(X,Code_b,Code_ab),
	name(AB,Code_ab).
tag(A,A_tag) :- 				%joins two words together as one atom
	name(A,Code_a),
	name('_',Code_), 
	append(Code_a,Code_,Code_tag), 
	name(A_tag,Code_tag).
%%-----------------------------------------------
addWord(W,T) :-					%if this is already a word, do nothing
	lookup_word(W,T),
	write('word already in memory'),nl,
	!.
addWord(W,T) :-					%else - add word 
	assertz(word(W,T,_,_)),
	write(W),nl,
	write('word added to memory'),nl.
%%%%%%%%% PRE-PROCESSING: WRD -> WORD %%%%%%%%%%%
spell_corrector([],[]).				%keep first & last letter & all other consonants (no vowels) in same order. 
spell_corrector([W|Rest],[C|Result]) :-
	spell_correct(W,C),
	spell_corrector(Rest,Result).
%%-----------------------------------------------
spell_correct(Correct,Correct) :-
	lookup_word(Correct,_),
	!.
spell_correct(Typo,Correct) :-
	(word(Correct,_,_,_); s(_,_,Correct,_,1,_)),
	typo2typ(Typo,Typ),
	typo2typ(Correct,Typ),
	!.
spell_correct(Name,Name) :-			% if fail to correct - assume a name - add word to dictionary as noun
	addWord(Name,n).
%%-----------------------------------------------
typo2typ(Typo,Typ) :-
	atom_chars(Typo,Typo_letters),
	doubles2singles(Typo_letters,Typo_leters),
	Typo_leters = [F|Rest],
	remove_vowels(Rest,Rst),
	atom_chars(Typ,[F|Rst]).
%%-----------------------------------------------
doubles2singles([],[]).
doubles2singles([L,L|Rest],[L|Result]) :-
	!,
	doubles2singles(Rest,Result).
doubles2singles([L|Rest],[L|Result]) :-
	!,
	doubles2singles(Rest,Result).
%%-----------------------------------------------
remove_vowels([],[]).
remove_vowels([y|Rest],[y|Result]) :-
	!,
	remove_vowels(Rest,Result).
remove_vowels([Vowel|Rest],Result) :-
	vowel(Vowel),
	!,
	remove_vowels(Rest,Result).
remove_vowels([Cons|Rest],[Cons|Result]) :-
	!,
	remove_vowels(Rest,Result).
remove_vowels(Last,Last).
%%%%%%%%%%%%%  DEPENDENCY PARSER %%%%%%%%%%%%%%%%
parse(List,HeadI) :- 
	make_chunk(List,ChunkI,ChunkD),
	length(ChunkI,N),
	succ(N,M),
	parse_node(M,ChunkD, [HeadD]),	
	parse_node(1,ChunkI, [HeadI]),
	HeadD = [_,_,_,T_dep,_,_], 		%join 2 chunks who have been parsed separately
	HeadI = [_,D,_,T_head,_,_],
	d(_,T_dep,T_head),			
	insert(HeadD,D),
	write('Semantic Chunk 1: '), write(ChunkI),nl,
	write('Semantic Chunk 2: '), write(ChunkD),nl.
parse(List,HeadI) :- 
	make_chunk(List,ChunkD,ChunkI),
	length(ChunkI,N),
	succ(N,M),
	parse_node(M,ChunkD, [HeadD]),	
	parse_node(1,ChunkI, [HeadI]),
	HeadD = [_,_,_,T_dep,_,_], 		%join 2 chunks who have been parsed separately
	HeadI = [_,D,_,T_head,_,_],
	d(_,T_dep,T_head),		
	insert(HeadD,D),
	write('Semantic Chunk 1: '), write(ChunkI),nl,
	write('Semantic Chunk 2: '), write(ChunkD),nl.
parse(List,HeadI) :-
	!, 
	parse_node(1,List,[HeadI]).
parse(_,[]) :-
	!. 	
%%-----------------------------------------------
						%% d(LR Word Ranges,Dep_word_type, Head_word_type)	%            _____rel
d([-10,0],v,rel). 				%	1) Relational words form head			%           /	     \
d([0,2],xv,v).					%	2) Aux Verbs attach to Verbs (to left)		%     r__ v___xv      v
d([-1,1],r,v).					%	3) Verb modifiers attach to verbs (closeby)	%        / \    
d([-10,3],n,v).					%	4) Nouns attach to verbs (closeby)		% a/s___n   n   
d([0,3],prep,n). d([0,2],det,n). d([0,1],xn,v). %	5) Nouns attach to Nouns (to left, closeby)	%       |         
d([0,1],a,n).	d([0,1],s,n).			%	6) Noun modifiers attach to Nouns (closeby)	%       n         
%%-----------------------------------------------
parse_node(_,_,[]).
parse_node(N,Sentence,Parse_tree) :-
	parse_loop(N,Sentence,[],[],Parse_tree).
%%-----------------------------------------------
parse_loop(A,[Word|Rest],Words,Heads,Parse_tree) :-
	lookup_word(Word,Type),	
	class_rel(Word,Class_rel),
	Node = [A,_,Word,Type,Class_rel,_],	%Node = [word_number, dependent_nodes, word, type, noun_class, attached_words]
	succ(A,B),	
	Words_mod = [Node|Words],
	parse_node(Node,Words,Heads,Heads_mod),
	parse_loop(B,Rest,Words_mod,Heads_mod,Parse_tree).
parse_loop(_,[],_,Heads,Heads).
%%-----------------------------------------------
parse_node(Node,[],[],[Node]) :-		%base condition
	!.
parse_node(Node,Words,Heads,Heads_mod2) :-
	make_head(Node,Heads,Heads_mod),
	make_dep(Node,Words,Heads_mod,Heads_mod2).
%%-----------------------------------------------
make_head(_,[],[]).				
make_head(Node,[Dep|Rest],Dep_mod) :-
	Node = [Nh,D,Wh,Type_node,Class_head,Linked],
	Dep = [N,_,W,Type_dep,Class_dep,_],
	d([L,R],Type_dep,Type_node),		
	X is Nh - N,
	X >= L,					
	X =< R,
	classify(Type_dep,W,X,Class_dep,Class_head),
	link_words(Type_dep,Wh,W,Linked),	
	insert(Dep,D),
	make_head(Node,Rest,Dep_mod).
make_head(Node,[Dep|Rest],[Dep|Deps_mod]) :-
	make_head(Node,Rest,Deps_mod).
%%-----------------------------------------------
make_dep(Node,Words,Heads,Heads) :-
	member(Head,Words),
	Node = [N,_,W,Type_node,Class_dep,_],
	Head = [Nh,D,Wh,Type_head,Class_head,Linked],
	d([L,R],Type_node,Type_head),		
	X is Nh - N,
	X >= L,					
	X =< R,	
	classify(Type_node,W,X,Class_dep,Class_head),
	link_words(Type_node,Wh,W,Linked),
	insert(Node,D).
make_dep(Node,_,Heads,[Node|Heads]).		%if node not dependent of any head, add as head to head_list_mod
%%-----------------------------------------------
class_rel(Word,Class) :-
	word(Word,rel,_,Class).
class_rel(_,_).
%%-----------------------------------------------
classify(n,_,N,subj,_) :-			%classify(Type,Word,Dist_dep2head,Dist_word2end,Classification).
	N > 0.
classify(n,_,N,dir_obj,_) :-
	N < 0.
classify(prep,to,_,_,ind_obj).
classify(prep,for,_,_,ind_obj).
classify(prep,on,_,_,locat).			
classify(prep,in,_,_,locat).
classify(prep,at,_,_,locat).
classify(prep,with,_,_,instr).
classify(prep,by,_,_,instr).
classify(prep,via,_,_,instr).
classify(_,_,_,_,_).
%%-----------------------------------------------
link_words(a,W_head,W_dep,Linked) :-		%adjectives
	Linked =.. [W_head,W_dep].
link_words(s,W_head,W_dep,Linked) :-		%adjectives
	Linked =.. [W_head,W_dep].
link_words(r,W_head,W_dep,Linked) :-		%adverbs
	Linked =.. [W_head,W_dep].
link_words(xv,W_head,W_dep,Linked) :-		%negations
	word(W_dep,_,_,neg),
	Linked =.. [W_head,W_dep].
link_words(_,_,_,_).
%%-----------------------------------------------
insert(Item,Slot) :-
	var(Slot),				%if slot is not filled "_" (empty)
	!,					
	Slot = [Item|_].			% insert the Node into it
insert(Item,[_|Slot]) :-			% if the slot is filled already
	insert(Item,Slot).			% recursively add that into next slot
%%-----------------------------------------------
functorise(Word,Type, Word_diff, Functor) :- 	%Word,VariableList --> Word(VariableList)
	Functor =.. [Type|[Word,Word_diff]], !.
%%-----------------------------------------------
make_chunk([],[],[]).	      			%make_chunk(input,rest,chunk).  %can only handle a sentence with 2 chunks!
make_chunk([W,so,that|Rest],Chunk,[W]) :-	
	!,					
	make_chunk([so,that|Rest],_,Chunk).
make_chunk([W,Rel|Rest],Chunk,[W]) :-		%e.g. make_chunk([i,like,you,because,i,like,ice,cream],X,Y).
	lookup_word(Rel,rel),			%X = [because, i, like, ice, cream],
	!,					%Y = [i, like, you].
	make_chunk([Rel|Rest],_,Chunk).
make_chunk([W,breakcomma|Rest],Chunk,[W]) :-		
	!,					
	make_chunk(Rest,_,Chunk).
make_chunk([Word|Rest], X, [Word|Rest_mod]) :-
	make_chunk(Rest,X,Rest_mod).	
%%%%%%%%%%%%%% PRINT PARSE TREE %%%%%%%%%%%%%%%%
write_list([First|Rest]) :-
	var(First),
	!,
   	write('_ '),
   	write_list(Rest).
write_list([First|Rest]) :-
   	write(First),
   	write(' '),
   	write_list(Rest).
write_list([]) :-
   	nl.
%%-----------------------------------------------
write_tree(Node) :-
	write_tree(Node,0).
write_tree([N,Dependents|Rest],Indentation) :-
	tab(Indentation),
	write(N), 
	write(' '), 
	write_list(Rest),
   	NewIndentation is Indentation + 3,
   	write_dep_open_list(Dependents,NewIndentation).
%%-----------------------------------------------
write_dep_open_list(X,_) :-
	var(X),
	!.
write_dep_open_list([First|Rest],N) :-
	write_tree(First,N),
	write_dep_open_list(Rest,N).	
%%%%%%%%%%%%  SEMANTIC NET  %%%%%%%%%%%%%%%%%%%%
semantic_net([_,[Dep1|[Dep2|_]],_,rel,temporal_seq,_],Time_before) :-	% temporal_before-after links
	init_globals,
 	succ(Time_before,Time_while), succ(Time_while,Time_after), succ(Time_after,Cause), succ(Cause,Effect), succ(Effect,Comparison), succ(Comparison,Contrast),
	loop_nodes(Dep1), verb(Verb), subj(Subj), dir_obj(Dobj), ind_obj(Iobj), instr(Instr), locat(Locat),
	Rule = axi(Verb, Subj, Dobj, Iobj, Instr, Locat,Time_before,Time_while,Time_after,Cause,Effect,Comparison,Contrast),
	nl,write(Rule),save(Rule),
 	succ(Contrast,Time_before2), succ(Time_before2,Time_while2), succ(Time_while2,Cause2), succ(Cause2,Effect2), succ(Effect2,Comparison2), succ(Comparison2,Contrast2),
	loop_nodes(Dep2), verb(Verb2), subj(Subj2), dir_obj(Dobj2), ind_obj(Iobj2), instr(Instr2), locat(Locat2),
	Rule2 = axi(Verb2, Subj2, Dobj2, Iobj2, Instr2, Locat2,Time_before2,Time_while2,Time_before,Cause2,Effect2,Comparison2,Contrast2),
	nl,write(Rule2),save(Rule2).
semantic_net([_,[Dep1|[Dep2|_]],_,rel,temporal_same,_],Time_before) :-	% temporal_same-time links
	init_globals,
 	succ(Time_before,Time_while), succ(Time_while,Time_after), succ(Time_after,Cause), succ(Cause,Effect), succ(Effect,Comparison), succ(Comparison,Contrast),
	loop_nodes(Dep1), verb(Verb), subj(Subj), dir_obj(Dobj), ind_obj(Iobj), instr(Instr), locat(Locat),
	Rule = axi(Verb, Subj, Dobj, Iobj, Instr, Locat,Time_before,Time_while,Time_after,Cause,Effect,Comparison,Contrast),
	nl,write(Rule),save(Rule),
 	succ(Contrast,Time_before2), succ(Time_before2,Time_after2), succ(Time_after2,Cause2), succ(Cause2,Effect2), succ(Effect2,Comparison2), succ(Comparison2,Contrast2),
	loop_nodes(Dep2), verb(Verb2), subj(Subj2), dir_obj(Dobj2), ind_obj(Iobj2), instr(Instr2), locat(Locat2),
	Rule2 = axi(Verb2, Subj2, Dobj2, Iobj2, Instr2, Locat2,Time_before2,Time_while,Time_after2,Cause2,Effect2,Comparison2,Contrast2),
	nl,write(Rule2),save(Rule2).
semantic_net([_,[Dep1|[Dep2|_]],_,rel,causal,_],Time_before) :-		% cause-effect links
	init_globals,
 	succ(Time_before,Time_while), succ(Time_while,Time_after), succ(Time_after,Cause), succ(Cause,Effect), succ(Effect,Comparison), succ(Comparison,Contrast),
	loop_nodes(Dep1), verb(Verb), subj(Subj), dir_obj(Dobj), ind_obj(Iobj), instr(Instr), locat(Locat),
	Rule = axi(Verb, Subj, Dobj, Iobj, Instr, Locat,Time_before,Time_while,Time_after,Cause,Effect,Comparison,Contrast),
	nl,write(Rule),save(Rule),
 	succ(Contrast,Time_before2), succ(Time_before2, Time_while2), succ(Time_while2,Time_after2), succ(Time_after2,Cause2), succ(Cause2,Comparison2), succ(Comparison2,Contrast2),
	loop_nodes(Dep2), verb(Verb2), subj(Subj2), dir_obj(Dobj2), ind_obj(Iobj2), instr(Instr2), locat(Locat2),
	Rule2 = axi(Verb2, Subj2, Dobj2, Iobj2, Instr2, Locat2,Time_before2,Time_while2,Time_after2,Cause2,Cause,Comparison2,Contrast2),
	nl,write(Rule2),save(Rule2).
semantic_net([_,[Dep1|[Dep2|_]],_,rel,compar,_],Time_before) :-		% comparison links
	init_globals,
 	succ(Time_before,Time_while), succ(Time_while,Time_after), succ(Time_after,Cause), succ(Cause,Effect), succ(Effect,Comparison), succ(Comparison,Contrast),
	loop_nodes(Dep1), verb(Verb), subj(Subj), dir_obj(Dobj), ind_obj(Iobj), instr(Instr), locat(Locat),
	Rule = axi(Verb, Subj, Dobj, Iobj, Instr, Locat,Time_before,Time_while,Time_after,Cause,Effect,Comparison,Contrast),
	nl,write(Rule),save(Rule),
 	succ(Contrast,Time_before2), succ(Time_before2,Time_while2), succ(Time_while2,Time_after2), succ(Time_after2,Cause2), succ(Cause2,Effect2), succ(Effect2,Contrast2),
	loop_nodes(Dep2), verb(Verb2), subj(Subj2), dir_obj(Dobj2), ind_obj(Iobj2), instr(Instr2), locat(Locat2),
	Rule2 = axi(Verb2, Subj2, Dobj2, Iobj2, Instr2, Locat2,Time_before2,Time_while2,Time_after2,Cause2,Effect2,Comparison,Contrast2),
	nl,write(Rule2),save(Rule2).
semantic_net([_,[Dep1|[Dep2|_]],_,rel,contr,_],Time_before) :-		% contrast links
	init_globals,
 	succ(Time_before,Time_while), succ(Time_while,Time_after), succ(Time_after,Cause), succ(Cause,Effect), succ(Effect,Comparison), succ(Comparison,Contrast),
	loop_nodes(Dep1), verb(Verb), subj(Subj), dir_obj(Dobj), ind_obj(Iobj), instr(Instr), locat(Locat),
	Rule = axi(Verb, Subj, Dobj, Iobj, Instr, Locat,Time_before,Time_while,Time_after,Cause,Effect,Comparison,Contrast),
	nl,write(Rule),save(Rule),
 	succ(Contrast,Time_before2), succ(Time_before2,Time_while2), succ(Time_while2,Time_after2), succ(Time_after2,Cause2), succ(Cause2,Effect2), succ(Effect2,Comparison2),
	loop_nodes(Dep2), verb(Verb2), subj(Subj2), dir_obj(Dobj2), ind_obj(Iobj2), instr(Instr2), locat(Locat2),
	Rule2 = axi(Verb2, Subj2, Dobj2, Iobj2, Instr2, Locat2,Time_before2,Time_while2,Time_after2,Cause2,Effect2,Comparison2,Contrast),
	nl,write(Rule2),save(Rule2).
semantic_net([_,Dep,Word,v,_,Att],Time_before) :-				% no links - single semantic chunk
	init_globals,
 	succ(Time_before,Time_while), succ(Time_while,Time_after), succ(Time_after,Cause), succ(Cause,Effect), succ(Effect,Comparison), succ(Comparison,Contrast),
	loop_nodes([_,Dep,Word,v,_,Att]), verb(Verb), subj(Subj), dir_obj(Dobj), ind_obj(Iobj), instr(Instr), locat(Locat),
	Rule = axi(Verb, Subj, Dobj, Iobj, Instr, Locat,Time_before,Time_while,Time_after,Cause,Effect,Comparison,Contrast),
	write(Rule), save(Rule).
semantic_net([_,Dep,Word,n,_,Att],Time_before) :-				% no links - single semantic chunk
	init_globals,
 	succ(Time_before,Time_while), succ(Time_while,Time_after), succ(Time_after,Cause), succ(Cause,Effect), succ(Effect,Comparison), succ(Comparison,Contrast),
	loop_nodes([_,Dep,Word,n,_,Att]), verb(Verb), subj(Subj), dir_obj(Dobj), ind_obj(Iobj), instr(Instr), locat(Locat),
	Rule = axi(Verb, Subj, Dobj, Iobj, Instr, Locat,Time_before,Time_while,Time_after,Cause,Effect,Comparison,Contrast),
	write(Rule), save(Rule).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
save(X) :-
	open('semantic_net.pl.',append,S), 
	write(S,X), 
	put_char(S,.),
	nl(S), 
	close(S). 		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 	
loop_nodes([_,Dep|Info]) :-		
	extract_globals(Info),			%access node - extract information
  	open_dep(Dep).				%go to all its dependent nodes 
%%-----------------------------------------------
open_dep(X) :-					%if no more dependents - backtrack
	var(X).
open_dep([First|Rest]) :-
	loop_nodes(First),
	open_dep(Rest).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 	%save global variables of subj,dobj,etc...
init_globals :-
    	retractall(verb(_)), assertz(verb(_)),
    	retractall(subj(_)), assertz(subj(_)),
    	retractall(dir_obj(_)), assertz(dir_obj(_)),
    	retractall(ind_obj(_)), assertz(ind_obj(_)),
    	retractall(instr(_)), assertz(instr(_)),
    	retractall(locat(_)), assertz(locat(_)).
%%-----------------------------------------------
extract_globals([Word,v,_,X]) :-			%verbs
	var(X),
    	retractall(verb(_)), assertz(verb(Word)).
extract_globals([_,v,_,Word]) :-			%verbs(adverbs)
    	retractall(verb(_)), assertz(verb(Word)).
extract_globals([Word,n,Class,X]) :-			% lone noun = subj
	var(X),
	var(Class),
	retractall(subj(_)), assertz(subj(Word)).
extract_globals([_,n,Class,Word]) :-			% lone noun(adjective)
	var(Class),
	retractall(subj(_)), assertz(subj(Word)).
extract_globals([Word,n,dir_obj,X]) :-			%if 2x dir_obj - make first ind_obj
	var(X),
	dir_obj(W), not(var(W)),			%if a direct object is already stored
	retractall(ind_obj(_)), assertz(ind_obj(W)), 	%resave it as indirect object
    	retractall(dir_obj(_)), assertz(dir_obj(Word)).
extract_globals([_,n,dir_obj,Word]) :-				
	dir_obj(W), not(var(W)),				
	retractall(ind_obj(_)), assertz(ind_obj(W)), 	
    	retractall(dir_obj(_)), assertz(dir_obj(Word)).
extract_globals([Word,n,Class,X]) :-			%store other noun classes
	var(X),
	All_global =.. [Class,_],
	Global =.. [Class,Word],
    	retractall(All_global),	assertz(Global).
extract_globals([_,n,Class,Word]) :-			%store other noun(adjectives)
	All_global =.. [Class,_],
	Global =.. [Class,Word],
    	retractall(All_global),	assertz(Global).
extract_globals(_).
%%%%%%%%%%%%%%% RULE CERTAINTY %%%%%%%%%%%%%%%%%%
calc_certainty([],Result,Result).		%base condition: if no words - then return same percentage
calc_certainty([Word|Rest],Source,Result) :-	%recursive: multiply fuzzy values of all words		
	word(Word,_,Y,_),
	Fuzzy is Source*Y,
	!,
	calc_certainty(Rest,Fuzzy,Result).
calc_certainty([_|Rest],Fuzzy,Result) :-	%if word cannot be found - skip it
	calc_certainty(Rest,Fuzzy,Result).			
%%%%%%%%%%%% Example sentences %%%%%%%%%%%%%%%%%%%
ex(1, 'jimmm built his granddaughter a castle on the beach').
ex(2, 'after michael took generous spoonfuls, he passed it to us').
ex(3, 'to explain the broken lamp, we dared not tell mom the truth').
ex(4,'tommmas paid a hundred pounds to the mechanic to fix the squeaky brakes').
ex(5,'so that darrren would have a friend at the party, sammmy brought him a date').
ex(6,'lessslie didnt have any money for a sandwich so sssmitty purchased it for her').
%%%%%%%%%%%% useful Meta-predicates %%%%%%%%%%%%%%
%clause(A,B)
%example(X) :- greeting(X). 
%clause(example(a), B).  %B = greeting(a).  