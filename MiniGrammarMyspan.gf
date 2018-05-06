concrete MiniGrammarMyspan of MiniGrammar = open MiniResMyspan, Prelude in {

  lincat
    Utt = {s : Str} ;
    Adv = Adverb ;
    Pol = {s : Str ; p : Polarity} ;
    S  = {s : Str} ;
    Cl = {s : Polarity => Str} ;
    VP = {v : Verb ; c : Agr => Str ; isAux : Bool} ;
    AP = Adjective ;
    CN = Noun ;
    NP = Pronoun ;
    Pron = Pronoun ;
    Det  = Determiner ;
    Conj = {s : Str} ;
    Prep = {s : Str ; c : Case} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = ProperName ;

  lin
    -- utterance:
    UttS s = s ;
    
    UttNP np = {s = np.s ! Nom} ;

    UsePresCl pol cl = {
      s = pol.s ++ cl.s ! pol.p ;
      } ;
    
    -- verbs:
    PredVP np vp = {
      s = \\p =>
         np.s ! Nom ++
         case p of {
	   Pos => vp.v.s ! np.a.n ! np.a.p ;
	   Neg => case vp.isAux of {
	     True  => vp.v.s ! np.a.n ! np.a.p ++ "not" ;
	     False => (do_V.s ! np.a.n ! np.a.p ++ "not" ++ vp.v.s ! Pl ! P3)
	            | (dont_V.s ! np.a.n ! np.a.p ++ vp.v.s ! Pl ! P3)
	     }
	   } ++
	 vp.c ! np.a ;
      } ;

    -- in chap 9:
  -- lin PredVP np vp =
    --  let                 -- why "let"?
    --    subj = (np.s ! Nom).obj ;
    --    obj = vp.obj ;
    --    clit = vp.clit ;
    --    verb = table {
    --    Pres => agrV vp.v np.a ;
    --    Perf => agrV (auxVerb vp.v.aux) np.a ++ agrPart vp.v np.a
    --    }
    --    in {
    --    s = \\t => subj ++ clit ++ verb ! t ++ obj
    --    } ;
    
    -- in book... UseV : V -> VP ????
                  lin UseV v = v

    UseV v = {
      v = v ;
      c = \\_ => [] ;
      isAux = False ;
      } ;
    
    ComplV2 v2 np = {
      v = v2 ;
      c = \\_ => v2.c.prep ++ np.s ! v2.c.c ;
      isAux = False ;
      } ;
    
    -- adjectives:
    UseAP ap = {
      v = copula ;
      c = \\a => ap.s ;
      isAux = True ;
      } ;
      
    AdvVP vp adv = vp ** {
      c = \\a => vp.c ! a ++ adv.s
      } ;
    
    -- determiners:
    DetCN det cn = {
      s = \\c => det.s ++ cn.s ! det.n ;
      a = {g = cn.g ; n = det.n ; p = P3} ;
      } ;
    
    -- pronouns:
    UsePN pn = {s = \\c => pn.s ; a = {g = pn.g ; n = Sg ; p = P3}} ;
    
    UsePron p = p ;
    
    -- nouns:
    MassNP cn = {
      s = \\c => cn.s ! Sg ;
      a = {g = cn.g ; n = Sg ; p = P3} ;
      } ;
    
    a_Det = mkDeterminer (pre {("a"|"e"|"i"|"o") => "an"  ; _ => "a"}) Sg ;
    
    aPl_Det = mkDeterminer "" Pl ;
    
    the_Det = mkDeterminer "the" Sg ;
    
    thePl_Det = mkDeterminer "the" Pl ;
    
    UseN n = n ;
    
    -- adjectives:
    AdjCN ap cn = {
      s = \\n => ap.s ++ cn.s ! n ;
      g = cn.g ;
      } ;

    PositA a = a ;

    -- prepositions:
    PrepNP prep np = {s = prep.s ++ np.s ! prep.c} ;

    -- conjunctions:
    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    
    PPos  = {s = "" ; p = Pos} ;
    PNeg  = {s = "" ; p = Neg} ;

    and_Conj = {s = "y"} ;
    or_Conj = {s = "o"} ;

    cada_Det = mkDeterminer "cada" Sg ;
    todo_Det = mkDeterminer "todo" Masc Sg ;
    toda_Det = mkDeterminer "toda" Fem Sg ;
    todos_Det = mkDeterminer "todos" Masc Pl ;
    todas_Det = mkDeterminer "todas" Fem Pl ;

    in_Prep = {s = "en" ; c = Acc} ;
    on_Prep = {s = "en" ; c = Acc} ;
    with_Prep = {s = "con" ; c = Acc} ;

    i_Pron = mkPronoun "yo" "me" Masc Sg P1 ;
    youSg_Pron = mkPronoun "tú" "you" Fem Sg P2  ;
    he_Pron = mkPronoun "él" "him" Masc Sg P3  ;
    she_Pron = mkPronoun "ella" "her" Fem Sg P3  ;
    wemasc_Pron = mkPronoun "nosotros" "us" Masc Pl P1  ;
    wefem_Pron = mkPronoun "nosotras" "us" Fem Pl P1  ;
    youPl_Pron = mkPronoun "ustedes" "you" Fem Pl P2 ;
    theymasc_Pron = mkPronoun "ellos" "them" Masc Pl P3 ;
    theymfem_Pron = mkPronoun "ellas" "them" Fem Pl P3 ;
    
}




abstract Grammar = {
    cat
      Cl ; NP ; VP ; AP ; CN ; Det ; N ; A ; V ; V2 ;
    fun
      PredVP : NP -> VP -> Cl ;
      ComplV2 : V2 -> NP -> VP ;
      DetCN : Det -> CN -> NP ;
      ModCN : CN -> AP -> CN ;
      UseV : V -> VP ;
      UseN : N -> CN ;
      UseA : A -> AP ;
      
      a_Det, the_Det : Det ; this_Det, these_Det : Det ;
      i_NP, she_NP, we_NP : NP ;
}
