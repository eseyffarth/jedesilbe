# Jede Silbe, a NaNoGenMo15 project

This is my first entry for [NaNoGenMo 2015](https://github.com/dariusk/NaNoGenMo-2015). The goal of that project is to write code that generates a 50 000 word novel. I decided to reuse my Prolog knowledge base that (more or less accurately) represents the restrictions of German phonotactics, and generate a text that contains every possible German syllable. For the knowledge base, I used the information provided by T. Alan Hall in _Phonologie: Eine Einführung_ (2000) and my intuition as a native speaker. The syllables are represented in [SAMPA](http://www.phon.ucl.ac.uk/home/sampa/), a machine-readable phonetic alphabet.

My result contains 60610 syllables. However, some details of assimilation are not included in the knowledge base: the syllable "penk" will be generated even though the n would be turned into a velar nasal in front of the k.

# Pretty output
I made a [website to display all German syllables](https://enigmabrot.de/NaNoGenMo15/jedesilbe/).