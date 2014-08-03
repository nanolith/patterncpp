#ifndef  BUTTON_HEADER_GUARD
# define BUTTON_HEADER_GUARD

#include <pattern/Observer.h>

/**
 * Forward declaration for class Button.
 */
class Button;

/**
 * The ButtonObserver interface provides observers with a set of events that can
 * be received by observing a Button.
 */
class ButtonObserver
{
public:
    /**
     * This delegate method is called when a button has been clicked.
     */
    virtual void onClicked(Button& b) = 0;
};

/**
 * Button provides a public method by which click events can be stimulated, and
 * implements the Observer pattern so that ButtonObservers can receive onClicked
 * events.
 */
class Button : public pattern::Observable<ButtonObserver>
{
public:
    /**
     * Stimulate a button press, and notify all observers that the button has
     * been clicked.
     */
    void click();
};

#endif //BUTTON_HEADER_GUARD
