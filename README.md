# Dictionary Replacer in Haskell

Recently in a Stack Builders coding dojo, we practiced [an exercise suggested by Corey Haines](http://www.confreaks.com/videos/104-aac2009-lightning-talk-under-your-fingers). In this exercise, you substitute any text surrounded by '$' signs with a dictionary substitution.

At the time we did this exercise in Ruby, and [I came up with an extremely imperative algorithm](https://gist.github.com/jsl/8985707). Later, I decided that I wanted to see how it felt to implement in Haskell. I came up with a purely functional, type-safe implementation using the powerful Parsec library in about the same amount of lines of code as the Ruby implementation that I wrote.

Implementing this exercise in Haskell, some things, like the `map`s that would come naturally in the Ruby implementation are still there. In the Ruby implementation I also avoided using regular expressions, but at the cost of readability as more conditionals crept into the code, and the amount of state that needed to be managed quickly increased. Using Parsec, the structure of the language becomes apparent, and I think errors would be much easier to detect visually than in the imperative version.

In [Corey's talk](http://www.confreaks.com/videos/104-aac2009-lightning-talk-under-your-fingers), he says,

> Take a half an hour a day, and practice the fundamentals of the language
> you're in. Practice using iterator methods. Practice using map. Practice
> using inject. Practice using some of these things so that when the time
> comes, you don't have to fall back on old ways.

This is certainly true, and it's interesting to also try implementing familiar algorithms in other languages to see how it makes you think about the task at hand. In this case, I think that the added safety of types, as well as the increased transparency of the language from Parsec is a big improvement.

## Usage

You must first `cabal install parsec` if you don't already have it. Then, you can execute `runghc DictionaryReplacer.hs` from the command line.

## LICENSE

MIT

## Author

Justin Leitgeb, Twitter: [@justinleitgeb](http://twitter.com/justinleitgeb)
