package org.morphir.sdk.core.platform

private[core] trait MaybeP[+A]
    extends IterableOnce[A]
    with Product
    with Serializable
