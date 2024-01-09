import Distribution.Simple
main = defaultMainWithHooks defaultUserHooks { hookedPrograms = [simpleUserHooks] }
