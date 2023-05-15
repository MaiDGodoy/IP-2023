{-# LANGUAGE PackageImports #-}
import Prelude
----- Completar con los datos del grupo

-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])


--------------------------------
---     FUNCIONES BÁSICAS    ---     
--------------------------------

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

--------------------------------
---        EJERCICIOS        ---
--------------------------------

---------------------------------------------------------------------------------------------------------------------------------------------
--toma la lista de usuarios de la red social y la pasa como argumento a la función proyectarNombres para obtener la lista de nombres de usuarios. 
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us, _, _) = proyectarNombres us 

--se encarga de filtrar los usuarios inválidos y de eliminar duplicados
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres ((_, nombre):us) = sinRepetidos(nombre : proyectarNombres us)
----------------------------------------------------------------------------------------------------------------------------------------------
--llama a la función amigosDe' y devuelve la lista de amigos resultante.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe redSocial usuario = amigosDe' (usuarios redSocial) (relaciones redSocial) usuario []

--recorre la lista de Usuarios, si un Usuario está relacionado directamente con el Usuario dado, lo agrega a la lista de amigos y continúa recorriendo
--Si no está relacionado, simplemente continúa recorriendo
amigosDe' :: [Usuario] -> [Relacion] -> Usuario -> [Usuario] -> [Usuario]
amigosDe' [] _ _ amigos = amigos
amigosDe' (u:us) relaciones usuario amigos
    | relacionadosDirecto u usuario redSocial = amigosDe' us relaciones usuario (u:amigos)
    | otherwise = amigosDe' us relaciones usuario amigos
    where redSocial = (us, relaciones, [])
----------------------------------------------------------------------------------------------------------------------------------------------
    
--toma una "red social" y un "usuario", y devuelve la cantidad de amigos que tiene ese usuario en esa red social.
--se llama a la función "longitud" con esa lista de amigos como argumento, lo que devuelve la cantidad de elementos en esa lista.
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos redSocial usuario = longitud (amigosDe redSocial usuario)
-----------------------------------------------------------------------------------------------------------------------------------------------
--La función principal devuelve el usuario de una red social que tiene más amigos.
--la función auxiliar se encarga  de comparar la cantidad de amigos de cda usuario de la lista
--utilizando llamada recursiva hasta q se de con el usuario con más amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us, rs, ps) = usuarioConMasAmigos' (tail us) (head us)
  where
    usuarioConMasAmigos' [] res = res
    usuarioConMasAmigos' (u:us) res
      | cantidadDeAmigos (us, rs, ps) u > cantidadDeAmigos (us, rs, ps) res = usuarioConMasAmigos' us u
      | otherwise = usuarioConMasAmigos' us res
-----------------------------------------------------------------------------------------------------------------------------------------------
--la función llama a la auxiliar, la cual se encarga de revisar si el primer usuario de la lista tiene más de 1000000 amigos, devolviendo True
--caso contrario,llama recursivamente a la función con la cola de la lista
--si es vacia, significa q no lo encontro, por lo tanto, False
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = existeRobertoCarlos red (usuarios red)

--determina si en esa lista de usuarios existe alguno que tenga más de 1.000.000 de amigos o si se cumple esta condición para algún usuario fuera de la lista.
existeRobertoCarlos :: RedSocial -> [Usuario] -> Bool
existeRobertoCarlos _ [] = False
existeRobertoCarlos red (u:us) =
    cantidadDeAmigos red u > 1000000 || existeRobertoCarlos red us
-----------------------------------------------------------------------------------------------------------------------------------------------
--devuelve una lista de publicaciones pertenecientes a un usuario específico
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe redSocial usuario = publicacionesDeUsuario (publicaciones redSocial) usuario
-- dentro de una red social dada. Itera sobre la lista de publicaciones y filtra aquellas cuyo usuario coincide con el usuario dado.

--realiza la recursión necesaria para construir la lista resultante. 
publicacionesDeUsuario :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeUsuario [] _ = []
publicacionesDeUsuario ((u, _, _):publicaciones) usuario
                   | u == usuario = (u,"",[]) : publicacionesDeUsuario publicaciones usuario
                   | otherwise = publicacionesDeUsuario publicaciones usuario
-- Si el usuario de la publicación coincide con el usuario pasado como argumento,
  -- se agrega la publicación a la lista de publicaciones y se continúa recursivamente con el resto de la lista.
 -- En caso contrario, es decir, si el usuario de la publicación no coincide con el usuario pasado como argumento,
  -- se continúa recursivamente con el resto de la lista de publicaciones sin agregar la publicación.
                
-----------------------------------------------------------------------------------------------------------------------------------------------
--devuelve una lista de publicaciones que le gustan a un usuario específico en una red social
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = sinRepetidos (publicacionesQueLeGustanAX (publicaciones red) u)

--filtra las publicaciones que le gustan a un usuario específico en una lista dada de publicaciones
publicacionesQueLeGustanAX :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAX [] _ = []
publicacionesQueLeGustanAX (pub:publicaciones) u
  | pertenece u (likesDePublicacion pub) = pub : publicacionesQueLeGustanAX publicaciones u --1
  | otherwise = publicacionesQueLeGustanAX publicaciones u                                  --2

   --1 Si el usuario 'u' pertenece a la lista de usuarios a los que les gusta la publicación 'pub',
   -- se agrega 'pub' a la lista de publicaciones y se continúa recursivamente con el resto de la lista.

   --2 En caso contrario, si el usuario 'u' no pertenece a la lista de usuarios que les gusta 'pub',
  -- se continúa recursivamente con el resto de la lista de publicaciones sin agregar 'pub'.
-----------------------------------------------------------------------------------------------------------------------------------------------
--determina si los usuarios u1 y u2 comparten las mismas publicaciones que les gustan en la red social red 
--utilizando la función publicacionesQueLeGustanA y comparando las listas resultantes con la función mismosElementos.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 =
  mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)
------------------------------------------------------------------------------------------------------------------------------------------------
--se encarga de iterar por todos los usuarios de la red social y evaluar si le han dado "Me gusta" a todas las publicaciones de autor
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red = existeSeguidorFiel (usuarios red) (publicaciones red)

--verifica si existe al menos un seguidor fiel en la red social.
existeSeguidorFiel :: [Usuario] -> [Publicacion] -> Usuario -> Bool
existeSeguidorFiel [] _ _ = False              --no hay más usuarios por revisar y no se ha encontrado un seguidor fiel
existeSeguidorFiel (u2:us) ps u | u /= u2 && likesDePublicacionSonUsuariosDeRed (likesDePublicacionesDeUsuario ps u) (u2:us) = True
                                | otherwise = existeSeguidorFiel us ps u

--primero se verifica que u no sea igual a u2
--luego se llama a la funcion  "likesDePublicacionesSonUsuariosDeRed" para verificar si todos los likes de las publicaciones en las que participa u son usuarios de la red social 
--si coinciden ambos devuelve True, sino llama a la cola de la lista                               

--obtiene todos los likes de las publicaciones en las cuales el usuario es el autor
likesDePublicacionesDeUsuario :: [Publicacion] -> Usuario -> [Usuario]
likesDePublicacionesDeUsuario [] _ = []
likesDePublicacionesDeUsuario (p:ps) u
      | usuarioDePublicacion p == u = likesDePublicacion p ++ likesDePublicacionesDeUsuario ps u
      | otherwise = likesDePublicacionesDeUsuario ps u
-------------------------------------------------------------------------------------------------------------------------------------------------
--comprueba si existe una secuencia de amistad que conecta dos usuarios en una red social
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2
            | u1 == u2 = True -- caso base: mismo usuario
            | relacionadosDirecto u1 u2 red = True -- caso base: usuarios relacionados directamente
            | otherwise = existeAmigoQueConectaCon u2 (amigosDe red u1) red --comprueba si existe un amigo en la lista de amigos de u1 que pueda conectar con u2 en la red social red

existeAmigoQueConectaCon :: Usuario -> [Usuario] -> RedSocial -> Bool
existeAmigoQueConectaCon u2 [] red = False -- caso base: no hay más amigos que buscar
existeAmigoQueConectaCon u2 (u:us) red
            | relacionadosDirecto u u2 red = True -- caso base: amigo conecta con u2 directamente
            | otherwise = existeAmigoQueConectaCon u2 us red -- buscar en la lista de amigos restantes
 
--------------------------------
---        AUXILIARES        ---
--------------------------------
--falta ordenar 

pertenece :: (Eq a) => a ->[a] -> Bool
pertenece e[]= False
pertenece e(x:xs) = e == x || pertenece e xs

mismosElementos :: (Eq a) => [a]-> [a]-> Bool
mismosElementos [][]=True
mismosElementos _[]=False
mismosElementos e(x:xs)=pertenece x e  && (mismosElementos xs) (quitar x e)
    where quitar _[]=[]
          quitar e(y:ys)
                |e == y = quitar e ys
                |otherwise = y:quitar e ys
 
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
    | pertenece x xs = sinRepetidos xs
    | otherwise = x : sinRepetidos xs

empiezaCon :: Eq a => a -> [a] -> Bool
empiezaCon _ [] = False
empiezaCon e (x:_) = e == x

terminaCon :: Eq a => a -> [a] -> Bool
terminaCon _ [] = False
terminaCon e [x] = e == x                --verifica si la lista tiene solo un elemento, en cuyo caso verifica si ese elemento es igual al elemento buscado
terminaCon e (x:xs) = terminaCon e xs    --Si la lista tiene más de un elemento, la función se llama a sí misma con la cola de la lista  como segundo argumento.

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = (u1,u2) `pertenece` relaciones red || (u2,u1) `pertenece` relaciones red --verifica si los usuarios(u1,u2) están relacionados directamente en la red social 'red'
                                                                                                         --se utiliza la función 'pertenece' para verificar esa relación


sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [] = True                                                 --caso base: lista vacia
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us  --se toma a el 1er usuario (u) de la lista para verificar q pertenezca
                                                                         --si 'u' pertenece,se llama recursivamente a la función sonDeLaRed con el resto de la lista 'us'

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True                             -- Caso base: lista vacía
noHayPublicacionesRepetidas [_] = True                            -- Caso base: una publicación en la lista
noHayPublicacionesRepetidas ((u1, _, _):(u2, _, _):ps)            -- Caso recursivo: dos publicaciones en la lista
    | idDeUsuario u1 == idDeUsuario u2 = False                    -- Si los usuarios son iguales, hay una publicación repetida
    | otherwise = noHayPublicacionesRepetidas ((u1,"", []) : ps) -- Sigue verificando la lista sin la segunda publicación

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] _ = True
cadenaDeAmigos [_] _ = True
cadenaDeAmigos (u1:u2:us) red = relacionadosDirecto u1 u2 red && cadenaDeAmigos (u2:us) red

usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos _ [] = True
usuariosLikeValidos [] _ = False
usuariosLikeValidos usl (u:us) = pertenece u usl && usuariosLikeValidos usl us

--verifica si todas las relaciones en una lista son asimétricas
--si (u1, u2) es una relación, entonces (u2, u1) no es una relación en la misma secuencia
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas ((u1,u2):rs) = not (pertenece (u2,u1) rs) && relacionesAsimetricas rs

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas ((u1,u2):rs) = noHayRelacionesRepetidasAux (u1,u2) rs && noHayRelacionesRepetidas rs
  where noHayRelacionesRepetidasAux _ [] = True
        noHayRelacionesRepetidasAux (u1,u2) ((u1',u2'):rs) = 
          (idDeUsuario u1 /= idDeUsuario u1' || idDeUsuario u2 /= idDeUsuario u2') &&
          (idDeUsuario u1 /= idDeUsuario u2' || idDeUsuario u2 /= idDeUsuario u1') &&
          noHayRelacionesRepetidasAux (u1,u2) rs

longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs                             --cuenta los elementos de la lista recursivamente

usuarioValido :: Usuario -> Bool
usuarioValido (id, nombre) = id > 0 && longitud nombre > 0   -- verificar que el id del usuario sea mayor a cero y que la longitud del nombre sea mayor a cero también.  

usuariosNoRepetidos :: RedSocial -> Bool
usuariosNoRepetidos (us, _, _) = noHayIdsRepetidos us

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (u:us) = idDeUsuarioNoEstaEnLista u us && noHayIdsRepetidos us

idDeUsuarioNoEstaEnLista :: Usuario -> [Usuario] -> Bool
idDeUsuarioNoEstaEnLista _ [] = True
idDeUsuarioNoEstaEnLista u (u':us') = idDeUsuario u /= idDeUsuario u' && idDeUsuarioNoEstaEnLista u us'
--idDeUsuarioNoEstaEnLista u (u':us') = not (idDeUsuario u == idDeUsuario u') && idDeUsuarioNoEstaEnLista u us'

usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (u:us) = usuarioValido u && noHayIdsRepetidos (u:us) && usuariosValidos us

likesDePublicacionSonUsuariosDeRed :: [Usuario] -> [Usuario] -> Bool
likesDePublicacionSonUsuariosDeRed [] _ = True
likesDePublicacionSonUsuariosDeRed (like:likes) usuariosDeRed = perteneceUsuario like usuariosDeRed && likesDePublicacionSonUsuariosDeRed likes usuariosDeRed

perteneceUsuario :: Usuario -> [Usuario] -> Bool
perteneceUsuario _ [] = False
perteneceUsuario u (u':us') = idDeUsuario u == idDeUsuario u' || perteneceUsuario u us'

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos [] _ = False
usuariosDeRelacionValidos us ((u1,u2):rs) =  pertenece u1 us && pertenece u2 us && u1 /= u2 && usuariosDeRelacionValidos us rs

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels = usuariosDeRelacionValidos us rels && relacionesAsimetricas rels && noHayRelacionesRepetidas rels

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed _ [] = True
usuariosDePublicacionSonUsuariosDeRed us (p:ps) =
  pertenece (usuarioDePublicacion p) us && usuariosDePublicacionSonUsuariosDeRed us ps

--publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
--publicacionesValidas us pubs = usuariosDePublicacionSonUsuariosDeRed us pubs && likesDePublicacionSonUsuariosDeRed us pubs && noHayPublicacionesRepetidas pubs

--redSocialValida :: RedSocial -> Bool
--redSocialValida red = usuariosValidos us &&
--                      relacionesValidas us rs &&
--                      publicacionesValidas us ps                               
