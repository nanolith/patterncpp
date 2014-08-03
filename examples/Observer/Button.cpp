#include "Button.h"

/**
 * Stimulate a button press, and notify all observers that the button has
 * been clicked.
 */
void
Button::click()
{
    //here, we pass a lamda expression to the notify method that calls the
    //onClicked method for each observer, passing this button in as the
    //argument.
    notify(
        [=](ButtonObserver& o) { o.onClicked(*this); });
}
