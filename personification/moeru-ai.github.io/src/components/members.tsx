import { Avatar, Skeleton, Tooltip } from '@radix-ui/themes'
import useSWR from 'swr'

/** @see {@link https://github.com/unjs/ungh#usersusername} */
interface User {
  avatar: string
  name: string
}

const Member = ({ member }: { member: string }) => {
  const navigate = (url: string) => () => window.open(url, '_blank', 'noopener')
  const usersUrl = 'https://ungh.cc/users/'

  // eslint-disable-next-line @masknet/no-then
  const { data, isLoading } = useSWR(new URL(member, usersUrl), async url => fetch(url, {
    headers: { accept: 'application/json' },
  })
    .then(async res => res.json() as Promise<{ user: User }>)
    .then(({ user }) => user))

  return (
    <Skeleton height="32px" loading={isLoading} width="32px">
      <Tooltip content={data?.name ?? member}>
        <Avatar
          className="hover:cursor-pointer"
          data-test-id={`member-${member}`}
          fallback={data?.name ?? member}
          onClick={navigate(`https://github.com/${member}`)}
          radius="full"
          size="2"
          src={data?.avatar}
        />
      </Tooltip>
    </Skeleton>
  )
}

export const Members = () => {
  const publicMembersUrl = 'https://api.github.com/orgs/moeru-ai/public_members'

  // eslint-disable-next-line @masknet/no-then
  const { data, isLoading } = useSWR(publicMembersUrl, async url => fetch(url, {
    headers: { accept: 'application/vnd.github+json' },
  })
    .then(async res => res.json() as Promise<{ login: string }[]>)
    .then(members => members.map(({ login }) => login)))

  return (
    <Skeleton loading={isLoading}>
      {data
      // eslint-disable-next-line sonarjs/pseudo-random
        ?.toSorted(() => Math.random() - 0.5)
        .map(member => (
          <Member key={member} member={member} />
        ))}
    </Skeleton>
  )
}
