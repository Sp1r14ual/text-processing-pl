# Обработка текста на языке Prolog

### Текст (Вариант 20)

```
На днях я принял участие в волонтерском проекте по помощи бездомным животным.
Мы собрали средства для приюта и помогли с уходом за животными там.
Работа была тяжелой, но мы чувствовали удовлетворение от того, что делаем что-то полезное для общества.
Местные жители поддержали нашу инициативу и принесли еду для животных.
Каждый волонтер ушел с чувством гордости за свою работу.
```

1. Запуск в [онлайне](https://swish.swi-prolog.org/)

   1.1. Скопировать [main.pl](main.pl), как есть, создать новую программу на swish и вставить код в редактор

   1.2. В окне с запросом (значок ?-) вставить запрос

   ```
   main.
   ```

   1.3. Нажать Run!

2. Запуск локально:

   2.1. Склонировать репозиторий (Опционально)

   ```
   git clone https://github.com/Sp1r14ual/project-management-system-pl.git
   ```

   2.2. **Закомментировать** в начале модуля [main.pl](main.pl) предикат

   ```
   % :- use_rendering(svgtree, [term(true), list(false)]).
   ```

   2.3. **Добавить** в начало модуля [main.pl](main.pl) предикат

   ```
   :- encoding(utf8).
   ```

   2.4. Скомпилить исполняемый файл

   ```
   swipl --goal=main --stand-alone=true -o main -c main.pl
   ```

   2.5. Запустить

   ```
   ./main
   ```
