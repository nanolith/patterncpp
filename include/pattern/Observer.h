/**
 * Observer.h.  C++11 Template Implementation of the Observer Pattern.
 *
 * Copyright (c) 2014 Justin Handville
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#ifndef  PATTERN_CPP_PATTERN_OBSERVER_HEADER_GUARD
# define PATTERN_CPP_PATTERN_OBSERVER_HEADER_GUARD
//this is C++ only.
#ifdef  __cplusplus

#include <algorithm>
#include <functional>
#include <memory>
#include <vector>

namespace pattern {

    /**
     * The Observable template interface allows any object that is composed of
     * or inherited from this interface to register and notify observers that
     * provide an implementation of the ObserverInterface.
     */
    template <
        typename ObserverInterface>
    class Observable
    {
    public:
        /**
         * Notification functions can be used to notify an observer of some
         * event.  This function will be called once per registered observer by
         * the notify method below.
         */
        typedef
        std::function<void (ObserverInterface&)>
        NotificationFunction;

        /**
         * Add an observer to this Observable.
         *
         * This method adds a weak reference to the Observable.  It's the
         * caller's responsibility to ensure that a strong reference to this
         * object is maintained for as long as notifications are required.  The
         * caller should call removeObserver() before the observer goes out of
         * scope to ensure best performance, but the interface will continue to
         * work if an exceptional condition leads to the observer falling out
         * of scope before it can be removed.
         */
        void addObserver(std::weak_ptr<ObserverInterface> observer);

        /**
         * Remove an observer from the list of observers.
         *
         * This method removes this obsserver from the list of observers if it
         * is still a valid observer.
         */
        void removeObserver(std::shared_ptr<ObserverInterface> observer);

        /**
         * For each valid observer, call the provided NotificationFunction,
         * passing the observer as a reference.
         *
         * This notification method provides a level of abstraction above the
         * standard Observer Pattern by allowing the caller -- typically the
         * observable object -- to pass in a lambda expression that calls the
         * expected interface method in the ObserverInterface for this
         * notification.  In this way, the Observable interface is completely
         * agnostic regarding interface details for the ObserverInterface, and
         * can be re-used as a generic pattern template.
         */
        void notify(NotificationFunction func);

    private:
        /**
         * The Observable object only keeps weak references to observers.  This
         * is done to prevent cyclic dependencies from forming.
         */
        typedef
        std::weak_ptr<ObserverInterface>
        ObserverPtr;

        /**
         * This container holds all registered observers.
         */
        std::vector<ObserverPtr> observers_;

        /**
         * Helper function to remove all observers that match the given
         * predicate.
         */
        void remove(std::function<bool (ObserverPtr&)> predicate);
    };

} /* namespace pattern */

#include <pattern/Observable.inl>

#endif //__cplusplus
#endif //PATTERN_CPP_PATTERN_OBSERVER_HEADER_GUARD
