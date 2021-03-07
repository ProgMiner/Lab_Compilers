# Разработка компиляторов
## Домашнее задание №2

Регулярное выражение: `(ab|bc)*cb`.

### Построение НКА
![NFA](/report/nfa.png)

### Построение ДКА
Таблица переходов НКА:

 № | Состояния | a | b | c
:-:|:---------:|:-:|:-:|:-:
1 | 1, 2, 3, 6, 10 | 4 | 7 | 11
2 | 4 | - | 2, 3, 5, 6, 9, 10 | -
3 | 7 | - | - | 2, 3, 6, 8, 9, 10
4 | 11 | - | 12 | -
5 | 2, 3, 5, 6, 9, 10 | 4 | 7 | 11
6 | 2, 3, 6, 8, 9, 10 | 4 | 7 | 11
__7__ | __12__ | - | - | -

Полученный ДКА:

![DFA](/report/dfa.png)

### Минимизация ДКА

Классы эквивалентности:

0) A<sub>0</sub> = { 1, 2, 3, 4, 5, 6 }, B<sub>0</sub> = { 7 }
1) A<sub>1</sub> = { 1, 5, 6 }, B<sub>1</sub> = { 2 }, C<sub>1</sub> = { 3 }, D<sub>1</sub> = { 4 }, E<sub>1</sub> = { 7 }
2) A<sub>2</sub> = { 1, 5, 6 }, B<sub>2</sub> = { 2 }, C<sub>2</sub> = { 3 }, D<sub>2</sub> = { 4 }, E<sub>2</sub> = { 7 }

Таблица переходов и хода минимизации:
<table>
    <thead>
        <tr>
            <th colspan="4"></th>
            <th colspan="3">P<sub>0</sub></th>
            <th colspan="3" align="center">P<sub>1</sub></th>
        </tr>
        <tr>
            <th>Состояние</th>
            <th>a</th>
            <th>b</th>
            <th>c</th>
            <th>a</th>
            <th>b</th>
            <th>c</th>
            <th>a</th>
            <th>b</th>
            <th>c</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td align="center">1</td>
            <td align="center">2</td>
            <td align="center">3</td>
            <td align="center">4</td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">B<sub>1</sub></td>
            <td align="center">C<sub>1</sub></td>
            <td align="center">D<sub>1</sub></td>
        </tr>
        <tr>
            <td align="center">2</td>
            <td align="center">&ndash;</td>
            <td align="center">5</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">A<sub>1</sub></td>
            <td align="center">&ndash;</td>
        </tr>
        <tr>
            <td align="center">3</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">6</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">A<sub>1</sub></td>
        </tr>
        <tr>
            <td align="center">4</td>
            <td align="center">&ndash;</td>
            <td align="center">7</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">B<sub>0</sub></td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">E<sub>1</sub></td>
            <td align="center">&ndash;</td>
        </tr>
        <tr>
            <td align="center">5</td>
            <td align="center">2</td>
            <td align="center">3</td>
            <td align="center">4</td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">B<sub>1</sub></td>
            <td align="center">C<sub>1</sub></td>
            <td align="center">D<sub>1</sub></td>
        </tr>
        <tr>
            <td align="center">6</td>
            <td align="center">2</td>
            <td align="center">3</td>
            <td align="center">4</td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">A<sub>0</sub></td>
            <td align="center">B<sub>1</sub></td>
            <td align="center">C<sub>1</sub></td>
            <td align="center">D<sub>1</sub></td>
        </tr>
        <tr>
            <td align="center"><strong>7</strong></td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
            <td align="center">&ndash;</td>
        </tr>
    </tbody>
</table>

Таблица переходов минимального ДКА:
 № | Класс эквивалентности | Состояния | a | b | c
:-:|:---------------------:|:---------:|:-:|:-:|:-:
 1 | A | 1, 5, 6 | 2 | 3 | 4
 2 | B | 2 | &ndash; | 1 | &ndash;
 3 | C | 3 | &ndash; | &ndash; | 1
 4 | D | 4 | &ndash; | 5 | &ndash;
 __5__ | __E__ | __7__ | &ndash; | &ndash; | &ndash;

Минимальный ДКА:

![Minimal DFA](/report/min_dfa.png)

### Программа-распознаватель
[Main.hs](/Main.hs)

### Примеры работы
```
Welcome to the regular language "(ab|bc)*cb" recognizer!
Only put your test inside terminal:
(send EOF (Ctrl+D) to terminate)
ab
[BAD]  Input is not sentence of this language.
cd
[BAD]  Input is not sentence of this language.
cb
[GOOD] Input is sentence of this language!
abbccb
[GOOD] Input is sentence of this language!
abcb
[GOOD] Input is sentence of this language!
bccb
[GOOD] Input is sentence of this language!
abbc
[BAD]  Input is not sentence of this language.
Bye!
```
