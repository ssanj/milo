# Milo

## Building

```
stack build --fast --file-watch
```

## Ghci

```
stack ghci
```

## How to run

Milo requires that you create a [Twitter App](https://developer.twitter.com/en/apps), and set the following environmental variables on invocation:

```
clientKey=aaa clientSecret=bbb accessToken=ccc accessTokenSecret=ddd milo-exec
```