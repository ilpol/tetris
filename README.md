# project-template
![](https://github.com/cmc-haskell-2017/tetris/blob/polozov_individual_task/test/Gif.gif)
[![Build Status](https://travis-ci.org/cmc-haskell-2017/project-template.svg?branch=master)](https://travis-ci.org/cmc-haskell-2017/project-template)



## Сборка и запуск

Соберать проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec my-project
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, надо использовать команду

```
stack ghci
```

