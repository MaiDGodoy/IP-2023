#EJERCICIO 1

#1.
def pertenece_1(s, e):
    for i in s:
        if i == e:
            return True
    return False

def pertenece_2(s, e):
    return e in s

def pertenece_3(s, e):
    return any(i == e for i in s)

def pertenece_4(lista: List[int], elemento: int) -> bool:
    # range
    for i in range(0, len(lista)):
        elemento_de_la_lista: int = lista[i]

        if elemento == elemento_de_la_lista:
            return True
    
    return False

def pertenece_5(lista: List[int], elemento: int) -> bool:
    # while
    i: int = 0

    while i < len(lista):
        elemento_de_la_lista: int = lista[i]
        
        if elemento_de_la_lista == elemento:
            return True

        i += 1

    return False

def pertenece_6(lista: List[int], elemento: int) -> bool:
    # for in
    for elemento_de_la_lista in lista:
        if elemento_de_la_lista == elemento:
            return True

    return False


#------------------------------------------------------------------------
#2.
def divideATodos(s, e):
    for i in s:
        if i % e != 0:
            return False
    return True

def divideATodos(s, e):
    divisibles = [i for i in s if i % e == 0]
    return len(divisibles) == len(s)

#---------------------------------------------------------------------------
#3.

def sumaTotal(s):
    """
    Calcula la suma de todos los elementos de la secuencia s.
    """
    suma = 0
    for elemento in s:
        suma += elemento
    return suma


def sumaTotal2(s):
    """
    Calcula la suma de todos los elementos de la secuencia s.
    Utiliza un ciclo while.
    """
    suma = 0
    i = 0
    while i < len(s):
        suma += s[i]
        i += 1
    return suma

#----------------------------------------------------------------------------------
#4.

def ordenados(s):
    """
    Verifica si todos los elementos de la secuencia s están ordenados de forma ascendente.
    """
    for i in range(len(s) - 1):
        if s[i] >= s[i + 1]:
            return False
    return True

def ordenados2(s):
    """
    Verifica si todos los elementos de la secuencia s están ordenados de forma ascendente.
    Utiliza la función `sorted` de Python para obtener una lista ordenada.
    """
    sorted_s = sorted(s)
    return sorted_s == s

#--------------------------------------------------------------------------------
#5.

def palabraLongitudMayorA7(palabras):
    """
    Verifica si alguna palabra en la lista_palabras tiene una longitud mayor a 7.
    Devuelve True si se cumple la condición, False en caso contrario.
    """
    for palabra in palabras:
        if len(palabra) > 7:
            return True
    return False

#--------------------------------------------------------------------------------
#6.

def es_palindroma(cadena):
    """
    Verifica si una cadena de texto es palíndroma.
    Devuelve True si es palíndroma, False en caso contrario.
    """
    longitud = len(cadena)
    for i in range(longitud // 2):
        if cadena[i] != cadena[longitud - i - 1]:
            return False
    return True

#--------------------------------------------------------------------------------
#7.

def analizarFortalezaContrasena(contraseña):
    """
    Analiza la fortaleza de una contraseña y devuelve un string con el resultado.
    """
    longitud = len(contraseña)
    tiene_letra_minuscula = False
    tiene_letra_mayuscula = False
    tiene_digito = False

    for caracter in contraseña:
        if caracter.islower():
            tiene_letra_minuscula = True
        elif caracter.isupper():
            tiene_letra_mayuscula = True
        elif caracter.isdigit():
            tiene_digito = True

    if longitud > 8 and tiene_letra_minuscula and tiene_letra_mayuscula and tiene_digito:
        return "VERDE"
    elif longitud < 5:
        return "ROJA"
    else:
        return "AMARILLA"
    
#----------------------------------------------------------------------
#8.

def calcularSaldo(historial):
    """
    Calcula el saldo actual a partir de un historial de movimientos en una cuenta bancaria.
    """
    saldo = 0

    for movimiento in historial:
        tipo_movimiento = movimiento[0]
        monto = movimiento[1]

        if tipo_movimiento == 'I':
            saldo = saldo + monto
        elif tipo_movimiento == 'R':
            saldo = saldo - monto

    return saldo

#----------------------------------------------------------------------
#9.

def tieneTresVocalesDistintas(palabra):
    """
    Verifica si una palabra tiene al menos 3 vocales distintas.
    """
    vocales = {'a', 'e', 'i', 'o', 'u'}
    vocales_encontradas = set()

    for letra in palabra:
        if letra.lower() in vocales:
            vocales_encontradas.add(letra.lower())

        if len(vocales_encontradas) >= 3:
            return True

    return False


def tieneTresVocalesDistintas(palabra):
    """
    Verifica si una palabra tiene al menos 3 vocales distintas.
    """
    vocales = 'aeiou'
    vocales_encontradas = []

    for letra in palabra:
        if letra.lower() in vocales:
            if letra.lower() not in vocales_encontradas:
                vocales_encontradas.append(letra.lower())

        if len(vocales_encontradas) >= 3:
            return True

    return False

#----------------------------------------------------------------------
#EJERCICIO 2
#1.
def borrarPosicionesPares(lista):
    """
    Modifica la lista original, colocando ceros en las posiciones pares.
    """
    for i in range(len(lista)):
        if i % 2 == 0:
            lista[i] = 0
            
      
#----------------------------------------------------------------------
#2.
def nuevaListaPosicionesPares(lista):
    """
    Crea una nueva lista con las posiciones pares en cero.
    """
    nueva_lista = []
    for i in range(len(lista)):
        if i % 2 == 0:
            nueva_lista.append(0)
        else:
            nueva_lista.append(lista[i])
    return nueva_lista


#----------------------------------------------------------------------
#3.            
def eliminarVocales(cadena):
    """
    Elimina las vocales de una cadena de texto y devuelve la cadena resultante.
    """
    nueva_cadena = ""
    for caracter in cadena:
        if caracter.lower() not in "aeiou":
            nueva_cadena += caracter
    return nueva_cadena

#----------------------------------------------------------------------
#4.
def reemplazaVocales(s):
    """
    Reemplaza las vocales de una secuencia de caracteres por espacios vacíos y devuelve la secuencia resultante.
    """
    res = ""
    for i in range(len(s)):
        if s[i] in "aeiouAEIOU":
            res += ""
        else:
            res += s[i]
    return res

def reemplazaVocales2(s):
    """
    Reemplaza las vocales de una secuencia de caracteres por espacios vacíos y devuelve la secuencia resultante.
    """
    res = ""
    for c in s:
        if c in "aeiouAEIOU":
            res += ""
        else:
            res += c
    return res
#----------------------------------------------------------------------
#5.
def daVueltaStr(s):
    """
    Devuelve una secuencia de caracteres que es el reverso de la secuencia de entrada.
    """
    res = ""
    for i in range(len(s) - 1, -1, -1):
        res += s[i]
    return res

#----------------------------------------------------------------------
#EJERCICIO 3
#1.
def construirListaEstudiantes():
    """
    Solicita al usuario los nombres de los estudiantes hasta que se ingrese la palabra "listo".
    Devuelve una lista con todos los nombres ingresados.
    """
    estudiantes = []
    nombre = ""
    
    while nombre != "listo":
        nombre = input("Ingrese el nombre del estudiante (o 'listo' para terminar): ")
        if nombre != "listo":
            estudiantes.append(nombre)
    
    return estudiantes

def construirListaEstudiantes2():
    """
    Solicita al usuario los nombres de los estudiantes hasta que se ingrese la palabra "listo".
    Devuelve una lista con todos los nombres ingresados.
    """
    estudiantes = []
    
    while True:
        nombre = input("Ingrese el nombre del estudiante (o 'listo' para terminar): ")
        if nombre == "listo":
            break
        estudiantes.append(nombre)
    
    return estudiantes
#----------------------------------------------------------------------
#2.
def obtenerHistorialMonedero():
    """
    Solicita al usuario las operaciones a realizar en un monedero electrónico.
    Devuelve una lista con el historial de operaciones.
    """
    historial = []
    saldo = 0
    
    while True:
        operacion = input("Ingrese la operación a realizar (C = Cargar créditos, D = Descontar créditos, X = Salir): ")
        
        if operacion == "C":
            monto = float(input("Ingrese el monto a cargar: "))
            saldo += monto
            historial.append(("C", monto))
        
        elif operacion == "D":
            monto = float(input("Ingrese el monto a descontar: "))
            saldo -= monto
            historial.append(("D", monto))
        
        elif operacion == "X":
            break
        
        else:
            print("Operación inválida. Intente nuevamente.")
    
    return historial

#----------------------------------------------------------------------
#3.
import random

def jugarSieteYMedio():
    """
    Simula el juego de 7 y medio.
    Devuelve el historial de "cartas" obtenidas por el usuario.
    """
    historial = []
    acumulado = 0
    
    while True:
        carta = random.randint(1, 12)
        
        if carta == 8 or carta == 9:
            continue
        
        historial.append(carta)
        
        if carta >= 10:
            acumulado += 0.5
        else:
            acumulado += carta
        
        print("Carta obtenida:", carta)
        print("Acumulado:", acumulado)
        
        if acumulado > 7.5:
            print("¡Has perdido!")
            break
        
        opcion = input("¿Deseas seguir sacando cartas? (S/N): ")
        
        if opcion.upper() == "N":
            break
    
    return historial

def jugarSieteYMedio2():
    """
    Simula el juego de 7 y medio.
    Devuelve el historial de "cartas" obtenidas por el usuario.
    """
    historial = []
    acumulado = 0
    
    while True:
        carta = random.randint(1, 12)
        
        if carta == 8 or carta == 9:
            continue
        
        historial.append(carta)
        
        if carta == 10 or carta == 11 or carta == 12:
            acumulado += 0.5
        else:
            acumulado += carta
        
        print("Carta obtenida:", carta)
        print("Acumulado:", acumulado)
        
        if acumulado > 7.5:
            print("¡Has perdido!")
            break
        
        opcion = input("¿Deseas seguir sacando cartas? (S/N): ")
        
        if opcion.upper() == "N":
            break
    
    return historial

#----------------------------------------------------------------------
#EJERCICIO 4
#1.
def perteneceACadaUno(s, e):
    """
    Verifica si el elemento 'e' pertenece a cada sublista de 's'.
    Devuelve una lista de booleanos indicando la presencia del elemento en cada sublista.
    """
    res = []
    
    for sublist in s:
        res.append(pertenece(sublist, e))
    
    return res

#----------------------------------------------------------------------
#2.
def esMatriz(s):
    """
    Verifica si una lista de listas es una matriz.
    Devuelve True si la lista de listas cumple con las condiciones de una matriz, False en caso contrario.
    """
    if len(s) == 0:
        return False

    first_len = len(s[0])
    for sublist in s:
        if len(sublist) != first_len:
            return False

    return True

def esMatriz2(s):
    """
    Verifica si una lista de listas es una matriz válida.
    Devuelve True si todas las sublistas tienen la misma longitud, False en caso contrario.
    """
    if len(s) == 0:
        return False
    len_primera_sublista = len(s[0])
    for sublista in s:
        if len(sublista) != len_primera_sublista:
            return False
    return True

#----------------------------------------------------------------------
#3.
def filasOrdenadas(m):
    """
    Verifica si cada fila de una matriz está ordenada de forma ascendente.
    Devuelve una lista de booleanos, donde cada elemento indica si la fila correspondiente está ordenada.
    """
    res = []
    
    for fila in m:
        res.append(ordenados(fila))
    
    return res

#----------------------------------------------------------------------
#4.
import numpy as np

def elevarMatriz(matriz, p):
    """
    Eleva una matriz cuadrada al exponente p.
    Devuelve la matriz resultante.
    """
    matriz_elevada = matriz.copy()

    for _ in range(p - 1):
        matriz_elevada = np.dot(matriz_elevada, matriz)

    return matriz_elevada


def elevar_matriz_cuadrada2(d, p):
    """
    Eleva una matriz cuadrada de tamaño d a la potencia p.
    Devuelve la matriz resultante.
    """
    matriz = []

    # Generar matriz aleatoria de tamaño d
    for i in range(d):
        fila = []
        for j in range(d):
            fila.append(random.random())
        matriz.append(fila)

    # Elevar la matriz a la potencia p
    matriz_elevada = matriz.copy()
    for _ in range(1, p):
        matriz_temp = []
        for i in range(d):
            fila_temp = []
            for j in range(d):
                elemento = 0
                for k in range(d):
                    elemento += matriz[i][k] * matriz_elevada[k][j]
                fila_temp.append(elemento)
            matriz_temp.append(fila_temp)
        matriz_elevada = matriz_temp

    return matriz_elevada
