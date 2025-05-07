# Word Search Plus

Word Search Plus is a [word search](https://en.wikipedia.org/wiki/Word_search ) implementation with couple extra features.

* The boards can be randomly generated based on any appropriate JSON file (see the `extra/` directory for examples), both remote (by URL) and local.
* Definitions for every word
* Search by definitions or blind search, all with customizable probabilities
* Set specific word count, options to filter words by length.

Created as semestral project for the NI-AFP course ([syllabus in english](https://bk.fit.cvut.cz/en/predmety/00/00/00/00/00/00/06/15/75/p6157506.html ), [syllabus in czech](https://bk.fit.cvut.cz/cz/predmety/00/00/00/00/00/00/06/15/75/p6157506.html ) ).

### How to run

You don't have do download any code, you can simply navigate to its [GH Pages](https://dolezvo1.github.io/wordsearchplus/ ) in your browser.

Optionally, if you really want to, you can clone this repository, and then

```shell
# First run this:
npm install

# Then either:
npm run dev # <- this if you want a local development server, or
npm run build # <- this if you want a production build (will be located in `dist/`)
```
