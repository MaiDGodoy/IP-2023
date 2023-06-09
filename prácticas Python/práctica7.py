
#EJERCICIO 1
import math

def raizDe2():
    resultado = math.sqrt(2)
    resultado = round(resultado, 4)
    return resultado

def imprimir_hola():
    print('Hola')


# Llamada a las funciones
resultado_raiz = raizDe2()
print("La raíz cuadrada de 2 con 4 decimales es:", resultado_raiz)

imprimir_hola()

# 3. Imprimir un verso
def imprimir_verso():
    verso = "Este es un verso\n De una canción\n Que yo elijo\n Con emoción"
    print(verso)

# Llamada a la función imprimir_verso()
imprimir_verso()

# 4. Factorial de dos
def factorial_2():
    n = 2
    factorial = 1
    for i in range(1, n+1):
        factorial *= i
    return factorial

# Llamada a la función factorial_2() y almacenamiento del resultado
resultado_factorial_2 = factorial_2()
print("Factorial de 2:", resultado_factorial_2)

# 5. Factorial de tres
def factorial_3():
    n = 3
    factorial = 1
    for i in range(1, n+1):
        factorial *= i
    return factorial

# Llamada a la función factorial_3() y almacenamiento del resultado
resultado_factorial_3 = factorial_3()
print("Factorial de 3:", resultado_factorial_3)

def factorial_4():
    n = 4
    factorial = 1
    for i in range(1, n+1):
        factorial *= i
    return factorial
# Llamada a la función factorial_4() y almacenamiento del resultado
resultado_factorial_4 = factorial_4()
print("Factorial de 4:", resultado_factorial_4)

def factorial_5():
    n = 5
    factorial = 1
    for i in range(1, n+1):
        factorial *= i
    return factorial
# Llamada a la función factorial_5() y almacenamiento del resultado
resultado_factorial_5 = factorial_5()
print("Factorial de 5:", resultado_factorial_5)


#EJERCICIO 2

def imprimir_saludo(nombre: str):
    print("Hola " + nombre)

def raiz_cuadrada_de(numero: float) -> float:
    return math.sqrt(numero)    

def imprimir_dos_veces(estribillo: str):
    print(estribillo * 2)

def es_multiplo_de(n: int, m: int) -> bool:
    if m == 0:
        return True if n == 0 else False
    return n % m == 0

#Test
print(es_multiplo_de(12, 4))  
print(es_multiplo_de(12, 5))  

def es_par(numero: int) -> bool:
    return es_multiplo_de(numero, 2)

#Test
print(es_par(10))
print(es_par(15))

def cantidad_de_pizzas(comensales: int, min_cant_de_porciones: int) -> int:
    total_porciones = comensales * min_cant_de_porciones
    porciones_por_pizza = 8
    pizzas = total_porciones // porciones_por_pizza
    if total_porciones % porciones_por_pizza > 0:
        pizzas += 1
    return pizzas

#EJERCICIO 3

def alguno_es0(numero1: int,numero2: int) -> bool:
        return (numero1== 0) or (numero2== 0)

#Test
print(alguno_es0(0, 5))
print(alguno_es0(2, 4))
print(alguno_es0(3, 0)) 

def ambos_son0(numero1: int,numero2: int) -> bool:
    return (numero1 == 0) and (numero2 == 0)

#Test
print(ambos_son0(0, 5))
print(ambos_son0(2, 4))
print(ambos_son0(0, 0)) 

def es_nombre_largo(nombre) -> bool:
    longitud = len(nombre)
    return (longitud >= 3) and (longitud <= 8)

#test
nombre= "Eustavio"
print (es_nombre_largo(nombre))

def es_bisiesto(año) -> bool:
    return (año % 400 == 0) or ((año % 4 == 0) and (año % 100 != 0))

#Test
print(es_bisiesto(2000))  
print(es_bisiesto(2021)) 

#EJERCICIO 4

def peso_pino(altura):
    if altura <= 300:
        return altura * 3
    else:
        return 900 + (altura- 300) * 2
    
def es_peso_util(peso) -> bool:
    return peso >= 400 and peso <=1000
    


def sirve_pino(altura) -> bool:
    peso= peso_pino(altura)
    return es_peso_util(peso)

#Con composicion de funciones
def sirve_pino2(altura) -> bool:
    return es_peso_util(peso_pino(altura))

#EJERCICIO 5

#1.semiformal
#problema dobleDelNumeroPar (in numero:Z) :Z{
#requiere={True}
#asegura={Si el número es par, el resultado es el doble del número. Si el número es impar, el resultado es el mismo número.}}


#1.formal
#problema dobleDelNumeroPar (in numero:Z) :Z{
#requiere={True}
#asegura= {(numero mod 2 = 0) → (res = numero * 2)} ∧ {(numero mod 2 ≠ 0) → (res = numero)}}    

def doble_si_par(numero):
    if numero % 2 == 0:
        return numero * 2
    else:
        return numero

#2.semiformal
#problema valor_o_siguiente (in numero:Z) :Z{
#requiere={True}
#asegura={Si el número es par, el resultado es el mismo número.}
#asegura={Si el número es impar, el resultado es el número siguiente.}

#2.formal
#problema valor_o_siguiente (in numero: Z) : Z {
#requiere={True}
#asegura={(numero mod 2 = 0) → (res = numero)} ∧ {(numero mod 2 ≠ 0) → (res = numero + 1)}}

def valor_o_siguiente(numero):
    if numero % 2 == 0:
        return numero
    else:
        return numero + 1

#3.semiformal
#problema operacion_multiplo (in numero: Z):Z {
#requiere={True}
#asegura={Si el número es múltiplo de 3, el resultado es el doble del número.} 
#asegura={Si el número es múltiplo de 9, el resultado es el triple del número.} 
#asegura={si el numero no es mutiplo de 3 ni de 9, entonces devuelve el mismo número}   


#3.formal
#problema operacion_multiplo (in numero: Z): Z {
#requiere: {True}
#asegura: {res = (numero * 2) <-> (numero % 3 = 0) V res=(numero * 3) <-> (numero % 9 = 0) V res= "numero" -> (numero % 3 ≠ 0) ∧ (numero % 9 ≠ 0) }}   

def operacion_multiplo(numero):
    if   numero % 3 == 0:
        return numero * 2
    elif numero % 9 == 0:
        return numero * 3
    else:
        return numero
    
#4.semiformal
#problema analizar_nombre (in nombre: String): String {
#requiere={True}
#asegura={si la longitud es igual o mayor a 5, devolver "Tu nombre tiene muchas letras". En otro caso, devolver "Tu nombre tiene menos de 5 caracteres".}}    
    
#4.formal
#problema analizar_nombre (in nombre: String): String {
#requiere: {True}
#asegura: {res = "Tu nombre tiene muchas letras" <-> (|nombre| ≥ 5) V res="Tu nombre tiene menos de 5 caracteres" <-> (|nombre|<5)}}
  
def analizar_nombre(nombre):
    if len(nombre) >= 5:
        return "Tu nombre tiene muchas letras!"
    else:
        return "Tu nombre tiene menos de 5 caracteres"

#5.semiformal
#problema jubilacion_vacaciones (in genero: String, in edad: Z): String {
#requiere: {True}
#asegura:{si fuese género Femenino mayores de 60, genero Masculino mayores de 65 y gente menores de 18 años, devuelve = "Anda de vacaciones"}
#asegura:{caso contrario, devuelve ="te toca trabajar"}}

#5.formal
#problema jubilacion_vacaciones (in genero: String, in edad: Z): String {
#requiere: {True}
#asegura:{res = "Anda de vacaciones" <-> (género = "F" ∧ edad > 60)∧ (género ="M", edad > 65) ∧ (edad < 18) }
#asegura:{res=  "Te toca trabajar"   <-> (género = "M" ∧ edad < 65) ∧ (género = "F"∧ edad <60)}}


def jubilacion_vacaciones(género, edad):
    if (género == "F" and edad > 60) and (género== "M" and edad > 65) and (edad < 18):
        return "Anda de vacaciones"
    else:
        return "Te toca trabajar"

#EJERCICIO 6    
    
def imprimir_nums():
    numero=1
    while numero <= 10:
        print(numero)
        numero += 1

imprimir_nums()

def imprimir_pares():
    num = 10
    while num <= 40:
        if num % 2 == 0:
            print(num)
        num += 1

imprimir_pares()        

def imprimir_eco():
    contador = 1
    while contador <= 10:
        print("eco")
        contador += 1

imprimir_eco()   

def cuenta_regresiva(num):
    while num > 0:
        print(num)
        num -= 1
    print("Despegue")

num=5
cuenta_regresiva(num)    

def monitorear_viaje(partida, llegada): #revisar
    while partida > llegada:
        print("Viajo un año al pasado, estamos en el año:", partida)
        partida -= 1

def monitorear_viaje(partida):
    llegada = 384
    while partida > llegada:
        print("estamos en el año:",partida)
        partida -= 20
#revisar

#EJERCICIO 7

def imprimir_numeros():
    for num in range(1, 11):
        print(num)

def imprimir_pares():
    for num in range(10, 41, 2):
        print(num)

def imprimir_eco():
    for _ in range(10):
        print("eco")

def cuenta_regresiva(numero):
    for num in range(numero, 0, -1):
        print(num)
    print("Despegue")       

def monitorear_viaje(partida, llegada): #mal , rehacer
    for año in range(partida, llegada, -1):
        print("Viajo un año al pasado, estamos en el año:", año)                     

#EJERCICIO 8

#1.
#x=5
#y=7

#2.
#x=5
#y=7
#z=x+y
#--> z=12

#3.
#x=5
#x="hora"
#--> "hora" (se reescribe)

#4.
#x=True
#y=False
#True and False
#--> False

#5.
#x=False
#res=not(x)
#--> True
