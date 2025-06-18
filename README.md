# lyc-laboratorio

Laboratorio de la materia **Lenguajes y Compiladores (LyC)** de FAMAF-UNC que consiste en dar semántica para un lenguaje imperativo en Haskell.

## Descripción
En este proyecto se propone una semántica para programas de un lenguaje específico de dominio en Haskell. Los programas pueden estar compuestos de expresiones, asignaciones, condiciones, variables locales, manejo de fallos (`Lab1.hs`) y además de operaciones de entrada y salida (`Lab2.hs`).

Asimismo, cada archivo contiene ejemplos sencillos para probar su funcionamiento.

## Instalación y Uso de Ejemplos
Cloná el repositorio y accedé al directorio:

```
git clone https://github.com/FrancoArtico/lyc-laboratorio.git
cd lyc-laboratorio
```
### División entera
Podemos usar GHCi para ejecutar la división entera de la siguiente manera

```ghci
ghci> :load Lab1.hs
ghci> ejemploDivMod 8 2
x vale 4
y vale 0
```

lo cual debería imprimir el valor de la división (`x`) y el resto (`y`).
> ⚠️ Acá podemos reemplazar 8 y 2 por cualquier par enteros.

### Fibonacci para números positivos
Para ejecutar el ejemplo de la función Fibonacci usamos lo siguiente dentro de GHCi

```ghci
ghci> :load Lab2.hs
ghci> ejemploFib
```
> ⚠️ Ingresá un número positivo. En caso de ingresar un número negativo, la función imprimirá 0 y terminará inmediatamente.

esto esperará a recibir un valor entero y calculará el valor de la función Fibonacci para ese valor. 
