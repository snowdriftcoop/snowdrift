# Notes for the crowdmatch-remove-StripeI branch
The goal of this branch was to remove StripeI completely from the project and
go back to normal Stripe API calls as much as possible.  The following are
notes as to what went right and what went wrong in this unfinished branch.

I was successful in removing StripeI from the crowdmatch and website
executables.  What I essentially did was restructure all of the functions that
originally used StripeI and changed it to just take a standard StripeConfig
variable.  The little bit of code that was in the website directory wasn't
being used, but I was able to make it compile anyway.

Where I ran into troubles was with the tests for crowdmatch.  The request from
Bryan was to perform the testing *without* actually calling the Stripe API.
Even though Stripe does allow for testing, Bryan's concern was sharing our
username and password for Stripe with everyone who wanted to develop to perform
those tests.

My strategy to get around this ultimately resulted in a separate module being
created, Crowdmatch.Stripe, which had one function "stripe" that had the same
parameters as the real stripe function and then create a StripeConfig variable
within the test with a secretKey of "test".  The new stripe function would
check the StripeConfig variable for the secretKey of "test" and return dummy
data if so, and would call the real stripe function and return its results
if it was anything else (meaning a real Stripe call), so that it could work for
testing, production & dev.

Where I came into trouble, though, was generating the dummy data.  Stripe's API
is riddled with type trickery, including a ton of class instances and type
families.  I couldn't simply return a variable of type Customer (even though
Customer was an instance of StripeReturn CreateCustomer), nor could I tell from
the StripeRequest variable that the call was for CreateCustomer (this was
essentially invisible).  What I was preparing to do at the point of stopping
was to emulate the Stripe website:  Review the StripeRequest data to determine
what the request was and then generate a JSON variable with the appropriate
dummy data, then run that through the same functions as what the stripe-http-streams
lib does in the callAPI function within Web.Stripe.Client.HttpStreams (though
copied and pasted into the new function, as that particular part of the process
was wrapped into a bigger function that did more than I wanted it to do for
testing purposes).

Bryan had asked me to stop at this point, as he did not feel that our testing
code should have to go to this extreme to perform the tests.  At this point,
this work is on hold until we can figure out how we can move forward.

Possible solutions at this point:

1) Continue with my planned approach and emulate the website's Request and
Response handlers, extracting part of the callAPI function and inserting it
into my stripe function within Crowdmatch.Stripe.

2) Modify the stripe lib(s) as needed to be able to handle the tests (be it 
by removing some of the type trickery or breaking down some of the
functionality of stripe-http-streams into separate functions that we could then
call directly) and submit the patches upstream.

3) Modify the stripe lib(s) as needed (see #2 above) and maintain the revised
version ourselves on git.snowdrift.coop (assuming the maintainer of the lib
doesn't accept our patches).

4) Create a new Stripe lib for us to use that avoids what is currently
available, and write it so that it can work with our testing suite as we want
it to.


I'm sure everyone can agree that the preference would be #2, however I added the
other options in case the patch is not accepted.


# [Snowdrift.coop]

[![build status](https://git.snowdrift.coop/sd/snowdrift/badges/master/build.svg)](https://git.snowdrift.coop/sd/snowdrift/commits/master)

This is the software that runs the [Snowdrift.coop] website.
It uses the [Yesod web framework] and [Haskell] programming language.

Key links:

* [What is Snowdrift?]
* [Issues]
* [Specs]
* [Roadmap]

## Code repositories

Main repository: [git.snowdrift.coop] (running GitLab CE)

We also have a mirror on [GitHub] but prefer issues and merge requests at the
main location.

## Contributing

Snowdrift.coop welcomes contributions of all sorts.

For ways to contribute outside of coding, see our [how-to-help] wiki page.

For code contributions, our **[contributor guide]** covers all the steps in
detail (including notes about our development practices, basic setup, and
resources for those new to Git, Haskell, or even to programming in general). The
following is an abbreviated summary:

* Follow our [build instructions] to build and test the software.

* File and search issues/tickets in the [issues] sections at git.snowdrift.coop.

* Connect with us in whatever way is most efficient for you. All questions,
    feedback, help requests, etc. are welcome.

### Communicating with us

* Email us at <community@snowdrift.coop> to discuss your volunteer interests.

* Sign up on the [dev email list] (and perhaps [other email lists]), and send a
  message introducing yourself to the community.

* Visit `#snowdrift` on freenode.net [IRC] for live chat (or the bridged Matrix
  room at `#snowdrift:matrix.org`)

* All public discourse is covered by our [Code of Conduct]

License
-------

Except where specified otherwise, all Snowdrift code is licensed under the
[GNU Affero General Public License](LICENSE.md) as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Our text and graphics documents (non-program-code) are also licensed under
[CC BY-SA 4.0 International].

[build instructions]: BUILD.md
[CC BY-SA 4.0 International]: https://creativecommons.org/licenses/by-sa/4.0
[contributor guide]: CONTRIBUTING.md
[GitHub]: https://github.com/snowdriftcoop/snowdrift
[Haskell]: https://www.haskell.org/
[IRC]: https://wiki.snowdrift.coop/community/irc
[Issues]: https://git.snowdrift.coop/sd/snowdrift/issues
[Roadmap]: https://tree.taiga.io/project/snowdrift/epics
[Snowdrift.coop]: https://snowdrift.coop/
[What is Snowdrift?]: https://snowdrift.coop/about
[Yesod web framework]: http://www.yesodweb.com/
[dev email list]: https://lists.snowdrift.coop/mailman/listinfo/dev
[git.snowdrift.coop]: https://git.snowdrift.coop/sd
[how-to-help]: https://wiki.snowdrift.coop/community/how-to-help
[other email lists]: https://lists.snowdrift.coop/
[Specs]: SPECS-STORIES.md
[Code of Conduct]: https://wiki.snowdrift.coop/community/conduct
