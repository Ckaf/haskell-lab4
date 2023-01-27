# haskell-lab4

## Задача
 - Сопоставление ключей use и dox файлов
 - Генерация markdown при нахождении несовпадений 
 - Генерация use файлов из dox файлов

## Ключи
- -dox - указать путь до .dox файла
- -use - указать путь до .use файла
- -md  - указать путь до markdown файла, который будет сгенерирован при несовпадении ключей(по умолчанию diff.md)
- -gu  - указать путь до .use файла, который будет сгенерирован из dox файла (по умолчанию generated.use)


## Описание форматов исследуемых файлов

### Dox format
#### Синтаксис
Пример:
```mrouted [-c конфигурационный_файл] [-d уровень_отладки] [-p]```

#### Опции
В блоке описания опций. 

- Короткая опция без аргумента:
```
@param -p

Описание опции
```
- Длинная опция без аргумента:
```
@param \--p

Описание опции
```
- Короткая опция с аргументом:
```
@param -p, аргумент

Описание опции
```
- Длинная опция с аргументом:
```
@param \--p, аргумент

Описание опции
```

- Короткая опция с аргументом (альтернативный формат):
```
@param -p=аргумент

Описание опции
```

- Длинная опция с аргументом (альтернативный формат):
```
@param \--p=аргумент

Описание опции
```
#### Описание:
Бла-бла-бла

### Use format

```
mrouted [ -p ] [ -c config_file ] [ -d debug_level ]

Options:
-p      Start mrouted in a non-pruning mode.
-c      Specify a configuration file (default /etc/mrouted.conf).
-d      Specify debug level (default 0).

Description:
Бла-бла-бла
```

## Пример результата
Markdown описанный ниже был сгенерирован из файлов `/test_files/left.dox` и `/test_files/left.use`.

Каждый из приведенных файлов имеет несовпадения между syntax и options секциями, 
сравнивать между собой эти файлы бессмысленно.

### Полученный md

*The options of the use syntax are not the same as those of the use options!*
```
| use syntax | use options |  Commentary |
|------------|-------------|-------------|
|     c      |      c      |     OK      |
|     d      |      d      |     OK      |
|     -      |      p      |   Failed    |
```
*The options of the dox syntax are not the same as those of the dox options!*
```
| dox syntax | dox options |  Commentary |
|------------|-------------|-------------|
|     c      |      c      |     OK      |
|     d      |      -      |   Failed    |
|     p      |      p      |     OK      |
```
