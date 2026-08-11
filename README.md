# lemurtype

lemurtype was inspired by monkeytype. It was designed to help you learn. It times how long it takes you to type each word and also remembers which words you make mistakes on, and then offers up those words more often when creating typing tests.

## Installation

Install [Crystal](https://crystal-lang.org/install/)

```
shards install
crystal build src/lemurtype --release
```

## Usage

`./lemurtype`

## Config

What the fields mean in the config.yaml file:

 - word_list: the text file to pull words from. one word per line.
 - test_length: how many words to put into a test
 - default_difficulty: for words you haven't typed yet what score to give them. if this is low you'll get more words you're slow at in each test. if this is high you'll get more new words you haven't seen yet. once you have typed all words at least once this does nothing.

## Contributing

1. Fork it (<https://github.com/cyanophage/lemurtype/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [Cyanophage](https://github.com/cyanophage) - creator and maintainer
