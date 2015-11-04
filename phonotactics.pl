%%%%%%%%%%%%%%%%%%%%%%% Section 0 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALL THIS PREDICATE TO WRITE A FILE WITH ALL POSSIBLE SYLLABLES       %

fileout :- open('syls.txt',write, Stream),
          (   syllable(S), syl_concat(S, Outstring), write(Stream, Outstring), fail;
              true
          ),
          close(Stream).

% Syllables are represented as atoms with trailing exclamation marks
syl_concat([H1, H2], R) :- atom_concat(H1, H2, S),
                           atom_concat(S, '! ', R).

syl_concat([H1, H2|T], R) :- atom_concat(H1, H2, P),
                             syl_concat([P|T], R).

%%%%%%%%%%%%%%%%%%%%%%% Section 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALL THIS PREDICATE TO SEGMENT AN INPUT WORD OR PHRASE INTO SYLLABLES %

segment([H|T],Output) :- not(H == 's' ; H == 'N'), validphrase([H|T], Output).


%%%%%%%%%%%%%%%%%%%%%%% Section 2 %%%%%%%%%%%%%%%%%%%%%%%
% Knowledge base: declaration of individual sounds and the classes they belong to %

/* PLOSIVES */
plosive('p').
plosive('b').
plosive('t').
plosive('d').
plosive('k').
plosive('g').
plosive('?').

/* FRICATIVES */
fricative('f').
fricative('v').
fricative('s').
fricative('z').
fricative('S').
fricative('Z').
fricative('C').
fricative('x').
fricative('h').

/* AFFRICATES */
affricate('pf').
affricate('ts').
affricate('tS').

/* OBSTRUENTS */
obstruent(S) :-         plosive(S) ;
                        fricative(S) ;
                        affricate(S).
                                                
/* ADDITIONAL CASES: UNVOICED OBSTRUENTS */
obsUnvoiced('p').
obsUnvoiced('t').
obsUnvoiced('k').
obsUnvoiced('f').
obsUnvoiced('s').
obsUnvoiced('S').
obsUnvoiced('C').
obsUnvoiced('x').
obsUnvoiced('?').

/* NASALS */
nasal('m').
nasal('n').
nasal('N').

/* LIQUIDS */
liquid('l').
liquid('j').
liquid('r').

/* SONORANTS */
sonorant(S) :-  nasal(S) ;
                liquid(S).

/* DIPHTHONGS */
diphthong('aI').
diphthong('aU').
diphthong('OI').

/* VOWELS */
vowel('i').
vowel('y').
vowel('e').
vowel('2').
vowel('E:').
vowel('u').
vowel('o').
vowel('a:').

vowel('I').
vowel('Y').
vowel('9').
vowel('E').
vowel('U').
vowel('O').
vowel('a').
vowel('@').

%%%%%%%%%%%%%%%%%%%%%%% Section 3 %%%%%%%%%%%%%%%%%%%%%%%
% Functionality: declaration of rules that correspond to phonotactic constraints %

/* GENERAL RULES FOR THE STRUCTURE OF PHRASES AND SYLLABLES */
validphrase([],L) :-         false.
validphrase(Input,L) :-      not(Input == []), syllable(Input), L=Input.
validphrase(Input,L1) :-     not(Input == []), not(Rest==[]), append(First, Rest, Input), syllable(First), validphrase(Rest,L2),
                             append(First, ['#'], FirstPlus), append(FirstPlus, L2, L1).


syllable([H|T]) :- shortOnset(H), rhyme(T). /* for one-consonant onsets */
syllable([H1,H2|T]) :-  (obsLiqOnset([H1,H2]) ; obsNasOnset([H1,H2]) ; obsObsOnset([H1,H2])),
                        rhyme(T). /* for two-consonant onsets */
syllable([H1,H2,H3|T]) :-       longOnset([H1,H2,H3]), rhyme(T). /* for long onsets */

rhyme([H|T]) :- nucleus(H), coda(T).

/* DEFINITIONS OF ONSET TYPES */
onset(L) :- shortOnset(L) ; obsLiqOnset(L) ; obsNasOnset(L) ; obsObsOnset(L) ; longOnset(L).

shortOnset(S) :- obstruent(S) ; sonorant(S).

obsLiqOnset([S1, S2]) :-        obstruent(S1),
                                                        liquid(S2),
                                                        (       ((S1=='p'; S1=='b'; S1=='d'; S1=='k'; S1=='g'; S1=='pf'; S1=='f'; S1=='v'), (S2=='r'; S2=='l'));
                                                                ((S1=='f'; S1=='t'), S2=='r') ;
                                                                ((S1=='s'; S1=='S'), S2=='l')
                                                        ).
                                                        
obsNasOnset([S1, S2]) :-        obsUnvoiced(S1),
                                                        nasal(S2),
                                                        (S1 == 'k' , (S2 == 'n' ; S2 == 'm') ;
                                                        S1 == 'S' , (S2 == 'n' ; S2 == 'm')).
                                                        
obsObsOnset([S1, S2]) :-        obstruent(S1),
                                                        obstruent(S2),
                                                        (((S1 == 'k' ; S1 == 'ts' ; S1 == 'S') , S2 == 'v' );
                                                        (S1 == 'S', (S2 == 't' ; S2 == 'p' ; S2 == 'k'))).
                                                        
longOnset([S1, S2, S3]) :-      fricative(S1),
                                                        plosive(S2),
                                                        liquid(S3),
                                                        ((
                                                                (
                                                                (S1 == 'S', S2 == 'p' ) ; 
                                                                (S1 == 's', S2 == 'k')
                                                                ),
                                                                (S3 == 'r' ; S2 == 'l') 
                                                        );
                                                        (S1 == 'S', S2 == 't', S3 == 'r')).
                                        
                                        
/* RULES FOR VALID NUCLEUS STRUCTURES */
% diphthongs are entered as a single element, e.g. ['d', 'aI', 'n'] %

nucleus(S) :-   vowel(S) ;
                diphthong(S).

                                
/* RULES FOR VALID CODA STRUCTURES */
coda(List) :- List == [].
coda(List) :- shortCoda(List).
coda(List) :- sonObsCoda(List).
coda(List) :- sonSonCoda(List).
coda(List) :- obsObsCoda(List).
coda(List) :- longCoda(List).

/* DEFINITIONS OF CODA TYPES */
shortCoda([S]) :-       sonorant(S) ;
                        (obsUnvoiced(S), not(S == '?')).
                                        
sonObsCoda([S1, S2]) :- sonorant(S1),
                                                obsUnvoiced(S2),
                                                (       ((S1=='r'; S1=='l'),(S2=='C'; S2=='S'; S2=='s'; S2=='f'; S2=='k'; S2=='t'; S2=='p'));
                                                        ((S1=='n'; S1=='m'), (S2=='S'; S2=='s'; S2=='f'; S2=='t'));
                                                        ((S1=='n', S2=='C');
                                                        (S1=='m', S2=='p'))
                                                        ).
                                                
                                                
                                                
sonSonCoda([S1, S2]) :- liquid(S1),
                                                sonorant(S2),
                                                (       (
                                                        (S1 == 'r' ; S1 == 'l'),
                                                        (S2 == 'm' ; S2 == 'n') ) ;
                                                        (S1 == 'r', S2 == 'l')  ).
                                                
obsObsCoda([S1, S2]) :- obstruent(S1), 
                                                obstruent(S2),
                                                (       ((S1=='C'; S1=='S'; S1=='f'; S1=='k'; S2=='p'), (S2=='s'; S2=='t'));
                                                        ( S1 == 's', (S2 == 'k'; S2=='t'));
                                                        ( S1 == 'p', S2 == 'S');
                                                        ( S1 == 'x', S2 == 't')
                                                        ).
                                                
longCoda([S1,S2,S3]) :- S1\=S2, S1\=S3, S2\=S3,
                                                (sonObsCoda([S1,S2]) ; sonSonCoda([S1,S2]) ; obsObsCoda([S1,S2])),
                                                (obsUnvoiced(S3), not(S3 == '?')),
                                                not((obsUnvoiced(S2), obsUnvoiced(S3))).
                                                        
longCoda([S1,S2,S3,S4]) :-      S1\=S2, S1\=S3, S1\=S4,
							    S2\=S3, S2\=S4,
							    S3\=S4,
							    (sonObsCoda([S1,S2]) ; sonSonCoda([S1,S2]) ; obsObsCoda([S1,S2])),                                              
							    (obsUnvoiced(S3), not(S3 == '?'), obsUnvoiced(S4), not(S4 == '?')).                                                     