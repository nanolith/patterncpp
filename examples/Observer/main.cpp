#include <iostream>
#include <memory>
#include <ostream>

#include "Button.h"

using namespace std;

/**
 * This ButtonObserver logs clicks to the given output stream.
 */
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

/**
 * Run a simple demo of the Observer Pattern.
 */
int main(int argc, char* argv[])
{
    //create a button
    auto button = make_shared<Button>();

    //create an observer that logs clicks to standard output.
    auto logger = make_shared<LoggingButtonObserver>(std::cout);

    //add the logger to the button as an observer
    button->addObserver(logger);

    //this should result in a message being sent to standard output.
    button->click();

    return 0;
}
