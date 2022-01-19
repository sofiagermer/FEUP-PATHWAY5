/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

interface Server {
    void run();

    SICStus getSICStus() throws InterruptedException;

    void stopServer();

}
