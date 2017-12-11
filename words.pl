:- dynamic word/4.
%word(Word, Type, Fuzzy, Additional Info).  %Additional Info for Verbs may be Instrument. For Nouns may be Plural or Subject, etc.

word(a,det,1.0,_).		%DETERMINERS (before Nouns)
word(an,det,1.0,_). 
word(the,det,1.0,_).

word(this,n,1.0,_).		%REFERENCE NOUNS (act like determiners)
word(that,n,1.0,_).
word(these,n,1.0,_).
word(those,n,1.0,_).

word(my,det,1.0,_).		%POSSESSIVE NOUNS
word(your,det,1.0,_).
word(our,det,1.0,_).		
word(his,det,1.0,_).
word(their,det,1.0,_).
				
word(no,xn,-1.0,neg).		%NEGATIONS (before Verbs or Nouns)
word(not,xv,-1.0,neg).
	
word(opinion,n,0.3,_).		%NOUNS
word(noone,n,1.0,_).
			
word(can,xv,0.5,_). 		%auxillary verbs (before Verbs)
word(can_,xv,0.5,_). 
word(could,xv,-0.7,_).
word(may,xv,0.1,_).
word(might,xv,0.1,_).
word(must,xv,-0.2,_).
word(shall,xv,-0.8,_).
word(should,xv,-0.9,_).
word(will,xv,-0.3,_).
word(would,xv,-0.7,_).
word(be_,xv,1.0,_).
word(is_,xv,1.0,_).
word(want_,xv,-0.7,_).
word(do_,xv,1.0,_).
word(did_,xv,1.0,_).
word(suppose,xv,0.2,_).
word(think,xv,0.3,mind).
word(guess,xv,0.2,mind).
word(reckon,xv,0.3,_).
word(seem,xv,0.7,senses).
word(dare_,xv,1.0,_). 


word(maybe,r,0.1,_).		%ADVERBS (before Verbs)
word(perhaps,r,0.1,_).
word(honestly,r,0.3,_).
word(frankly,r,0.3,_).
word(personally,r,0.3,_).
word(possibly,r,0.5,_).
word(probably,r,0.6,_).
word(apparently,r,0.7,_).

word(imagine,v,0.2,mind).	%VERBS
word(is,v,1.0,_).
word(be,v,1.0,_).
word(want,v,1.0,_).
word(do,v,1.0,_).
word(did,v,1.0,_).

word(you,n,1.0,_).
word(it,n,1.0,ref).

word(i,n,1.0,subj).		%SUBJECT NOUNS
word(we,n,1.0,subj).
word(he,n,1.0,ref).
word(she,n,1.0,ref).
word(they,n,1.0,ref).

word(me,n,1.0,obj).		%OBJECT NOUNS
word(us,n,1.0,obj).
word(her,n,1.0,ref).
word(him,n,1.0,ref).
word(them,n,1.0,ref).
word(mine,n,1.0,obj).
word(yours,n,1.0,obj).
word(ours,n,1.0,obj).
word(myself,n,1.0,obj).
word(yourself,n,1.0,obj).
word(yourselves,n,1.0,obj).
word(ourselves,n,1.0,obj).

word(what,q,1.0,_).		%QUESTION WORDS
word(who,q,1.0,_).
word(which,q,1.0,_).
word(whose,q,1.0,_).
word(whom,q,1.0,_).
word(why,q,1.0,_).
word(where,q,1.0,_).
word(when,q,1.0,_).
word(how,q,1.0,_).

word(and,conj,1.0,_).		%LOGIC WORDS
word(or,conj,1.0,_).
word(if,conj,1.0,_).
word(else,conj,1.0,_).
word(nor,conj,-1.0,_).		
				%RELATIONAL WORDS
word(because,rel,1.0,causal).
word(then,rel,1.0,temporal_seq).
word(so,rel,1.0,causal).
word(after,rel,1.0,temporal_seq).
word(cause_to,rel,1.0,causal).
word(equiv,rel,1.0,compar).

				%PREPOSITIONAL WORDS
word(on,prep,1.0,_).
word(in,prep,1.0,_).
word(at,prep,1.0,_).
word(of,prep,1.0,_).
word(for,prep,1.0,_).
word(to,prep,1.0,_).

word(breakcomma,special,1.0,_).	%SPECIAL WORDS
%%%%%%%%%%%%%%%%SUFFIX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
suffix(nt,neg,-1.0,_).
