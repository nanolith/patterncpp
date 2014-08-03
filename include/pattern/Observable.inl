/**
 * Observable.inl.  Implementation of generic methods for the Observable
 * interface.
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
#ifndef  PATTERN_CPP_PATTERN_OBSERVABLE_INL_HEADER_GUARD
# define PATTERN_CPP_PATTERN_OBSERVABLE_INL_HEADER_GUARD

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
template <
    typename ObserverInterface>
void
pattern::Observable<ObserverInterface>::addObserver(
    std::weak_ptr<ObserverInterface> observer)
{
    observers_.push_back(observer);
}

/**
 * Remove an observer from the list of observers.
 *
 * This method removes this obsserver from the list of observers if it
 * is still a valid observer.
 */
template <
    typename ObserverInterface>
void
pattern::Observable<ObserverInterface>::removeObserver(
    std::shared_ptr<ObserverInterface> observer)
{
    auto removePredicate =
        [=](ObserverPtr& ptr) -> bool
        {
            if (auto sharedPtr = ptr.lock())
            {
                //cull if the two match
                return sharedPtr == observer;
            }
            else
            {
                //cull because this weak reference is no longer valid.
                return true;
            }
        };

    remove(removePredicate);
}

/**
 * Helper function to remove all observers that match the given
 * predicate.
 */
template <
    typename ObserverInterface>
void
pattern::Observable<ObserverInterface>::remove(
    std::function<bool (ObserverPtr&)> predicate)
{
    auto endRange =
        std::remove_if(
            observers_.begin(),
            observers_.end(),
            predicate);

    observers_.erase(endRange, observers_.end());
}

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
template <
    typename ObserverInterface>
void
pattern::Observable<ObserverInterface>::notify(
    NotificationFunction func)
{
    for (auto i : observers_)
    {
        if (auto sharedPtr = i.lock())
        {
            func(*sharedPtr);
        }
    }
}

#endif //PATTERN_CPP_PATTERN_OBSERVABLE_INL_HEADER_GUARD
