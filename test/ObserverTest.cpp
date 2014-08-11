#include <gtest/gtest.h>
#include <iostream>
#include <mockpp/mockpp.h>
#include <pattern/Observer.h>

using namespace mockpp;
using namespace pattern;
using namespace std;

/**
 * Test observer interface.
 */
class TestObserver
{
public:
    virtual void onChange() = 0;
};

/**
 * Mock of the test observer interface.
 */
class MockObserver : public TestObserver, public Mock<TestObserver>
{
public:
    MOCK_FUNCTION_VOID(onChange);
};

/**
 * Adding an observer to an observable interface should allow the observer to
 * receive events.
 */
TEST(ObserverTest, addObserver)
{
    auto observable = make_shared<Observable<TestObserver>>();
    auto obs = make_shared<MockObserver>();

    //add the observer to this object.
    observable->addObserver(obs);

    //notify the observer.
    observable->notify([=](TestObserver& o) { o.onChange(); });

    //the observer should have been notified.
    EXPECT_TRUE(VALIDATE(*obs, onChange).called());
}

/**
 * Adding multiple observers should notify each one during a notification.
 */
TEST(ObserverTest, addObserverMultiple)
{
    auto observable = make_shared<Observable<TestObserver>>();
    auto obs1 = make_shared<MockObserver>();
    auto obs2 = make_shared<MockObserver>();

    //add the observers to this object.
    observable->addObserver(obs1);
    observable->addObserver(obs2);

    //notify the observers.
    observable->notify([=](TestObserver& o) { o.onChange(); });

    //the observers should have been notified.
    EXPECT_TRUE(VALIDATE(*obs1, onChange).called());
    EXPECT_TRUE(VALIDATE(*obs2, onChange).called());
}

/**
 * After an observer is removed, it should no longer receive notifications.
 */
TEST(ObserverTest, removeObserver)
{
    auto observable = make_shared<Observable<TestObserver>>();
    auto obs = make_shared<MockObserver>();

    //add the observer to this object.
    observable->addObserver(obs);

    //notify the observers.
    observable->notify([=](TestObserver& o) { o.onChange(); });

    //the observer should have been notified.
    EXPECT_TRUE(VALIDATE(*obs, onChange).called());

    //remove the observer.
    observable->removeObserver(obs);

    //notify the observers.
    observable->notify([=](TestObserver& o) { o.onChange(); });

    //the observer should NOT have been notified.
    EXPECT_FALSE(VALIDATE(*obs, onChange).called());
}

/**
 * Removing an observer should not crash if one or more observers no longer
 * exist.
 */
TEST(ObserverDeathTest, removeObserver)
{
    auto observable = make_shared<Observable<TestObserver>>();

    auto obs = make_shared<MockObserver>();

    //add the observer to this object.
    observable->addObserver(obs);

    //force the observer to fall out of scope.
    obs.reset();

    //remove the observer.  This should not crash.
    EXPECT_EXIT(observable->removeObserver(obs); exit(0), ::testing::ExitedWithCode(0), ".*");
}

/**
 * Notifying an observer that no longer exists should not cause a crash.
 */
TEST(ObserverDeathTest, notifyObserver)
{
    auto observable = make_shared<Observable<TestObserver>>();

    auto obs = make_shared<MockObserver>();

    //add the observer to this object.
    observable->addObserver(obs);

    //force the observer to fall out of scope.
    obs.reset();

    //notify observers.  This should not crash.
    EXPECT_EXIT(observable->notify([=](TestObserver& o) { o.onChange(); }); exit(0), ::testing::ExitedWithCode(0), ".*");
}
