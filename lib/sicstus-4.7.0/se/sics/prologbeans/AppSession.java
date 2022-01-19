/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;

class AppSession implements HttpSessionBindingListener {

    private final PrologSession session;

    public AppSession(PrologSession session) {
        this.session = session;
    }

    public PrologSession getPrologSession() {
        return session;
    }

    @Override
    public void valueBound(HttpSessionBindingEvent event) {

    }

    @Override
    public void valueUnbound(HttpSessionBindingEvent event) {

        // [PM] 4.2.1 FIXME: do we really have to verify the name?
        final String eventName = event.getName();
        if (PrologSession.SESSION_ATTRIBUTE_NAME.equals(eventName)) {
            session.endSession();
        } else {
            if (PrologSession.debugging()) {
                System.err.println("AppSession.valueUnbound(): unexpected event name: \"" + eventName + "\"");
                Thread.dumpStack();
            }
        }
    }

}
