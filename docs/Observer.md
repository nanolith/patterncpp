Observer Pattern
================

The Observer Pattern is described in the [Observer.h][observer-header] header
file, and implemented in the [Observable.inl][observable-inl] inline file.

The Observable template class is an interface by which observers which adhere to
the provided ObserverInterface interface can be added, removed, and notified of
important events.  Since this is a template implementation, Observable can be
customized to work with any observer interface.

The Observable template class makes use of std::weak\_ptr.  It is expected that
observers be allocated using std::make\_shared, and that a weak reference be
passed to the Observable.  In this way, circular references can be avoided, and
memory management concerns become easier to deal with.  Future versions of
Observable may expose further customization via type traits, but this will be
the default behavior of this implementation.

##Use Patterns

There are two ways to use the Observable template.  The first is to derive from
it (IS-A).  This is by far the simplest use case.  The second is to build a
class that has an internal Observable object (HAS-A).  In the former use case,
derived types automatically get the addObserver, removeObserver, and notify
methods.  On the other hand, since these methods are public, anyone can call
them.  This may or may not be desirable.

##Example

In this example, a Button provides onClicked() notifications to ButtonObservers,
using the Observable template.  This document provides relevant snippets of this
example.  The complete example is available in the [examples][observer-example]
directory.

In this example, we will first define a ButtonObserver interface that observers
can implement to get notifications.

```c++
    class ButtonObserver
    {
    public:
        virtual void onClicked(Button& b) = 0;
    };
```

We define a single onClicked callback method, and we include the Button that was
clicked as part of the method.  Now, we'll create a Button class that provides a
public stimulus method, click(), that kicks off the notification process.

```c++
    class Button : public pattern::Observable<ButtonObserver>
    {
    public:
        void click();
    };
```

The implementation of the click() method is simple.  We call the notify()
method, which repeatedly calls the provided lambda expression with each
observer.  It's up to the method calling notify() to plug in the details about
which callback method in the interface to call, which makes the Observable
pattern generic.  Since we can take advantage of C++11 lambda expressions, the
notification is also compact.

```c++
    void
    Button::click()
    {
        notify(
            [=](ButtonObserver& o) { o.onClicked(*this); });
    }
```

The notify() method expects a lambda expression that in turn expects a
ButtonObserver reference.  This lambda expression is then allowed call whichever
method is appropriate on the reference.

To put everything together, here is a simple logging class that logs every
button click to the given output stream.

```c++
    class LoggingButtonObserver : public ButtonObserver
    {
    public:
        LoggingButtonObserver(ostream& out)
            : out_(out)
        {
        }

        virtual void onClicked(Button& b)
        {
            out_ << "The button was clicked." << endl;
        }

    private:
        ostream& out_;
    };
```

Finally, here's a main() function that implements a button and calls click() on
it.  The result, which can be seen by running the exmaple, is that the logging
observer's onClicked method is called, which in turn logs the event to standard
output.

```c++
    int main(int argc, char* argv[])
    {
        auto button = make_shared<Button>();
        auto logger = make_shared<LoggingButtonObserver>(std::cout);

        //add the logger to the button as an observer
        button->addObserver(logger);

        //this should result in a message being sent to standard output.
        button->click();

        return 0;
    }
```

The complete example can be found in [here][observer-example].  This example is
built as part of the standard shake build.

[observer-example]: https://github.com/nanolith/patterncpp/blob/master/examples/Observer
[observer-header]: https://github.com/nanolith/patterncpp/blob/master/include/pattern/Observer.h
[observable-inl]: https://github.com/nanolith/patterncpp/blob/master/include/pattern/Observable.inl
